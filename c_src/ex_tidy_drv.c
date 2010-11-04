#include <errno.h>
#include <string.h>
#include <tidy/tidy.h>
#include <tidy/buffio.h>
#include <erl_nif.h>

typedef struct
{
    TidyAllocatorVtbl vtbl;
    TidyAllocator allocator;
    TidyDoc tdoc;
} tdoc_handle;

typedef struct {
    ErlNifBinary bin;
    ulong pos;
} tdoc_source_data;

typedef struct {
    TidyReportLevel lvl;
    uint line;
    uint col;
    ctmbstr msg;
} tdoc_report_entry;

#define DRV_FUN_NAME(name) \
    ex_tidy_drv_ ## name

#define EXTRACT_TDOC(handle, var, term) \
    TidyDoc var; \
    tdoc_handle *handle; \
    do { \
        void *objp; \
        if (!enif_get_resource(env, term, tdoc_resource, &objp)) { \
            return enif_make_badarg(env); \
        } \
        handle = (tdoc_handle *)objp; \
        var = handle->tdoc; \
    } while (0)

#define EXTRACT_STRING(var, term) \
    EXTRACT_STRING_OR_UNDEFINED_(var, term, no)

#define EXTRACT_STRING_OR_UNDEFINED(var, term) \
    EXTRACT_STRING_OR_UNDEFINED_(var, term, yes)

#define EXTRACT_STRING_OR_UNDEFINED_(var, term, allow_undefined) \
    tmbstr var = NULL; \
    do { \
        ErlNifBinary bin; \
        if (enif_inspect_iolist_as_binary(env, term, &bin)) { \
            var = enif_alloc(bin.size + 1); \
            strncpy(var, (char *)bin.data, bin.size); \
            var[bin.size] = '\0'; \
        } else if (!allow_undefined || \
                !enif_is_identical(term, enif_make_atom(env, "undefined"))) { \
            return enif_make_badarg(env); \
        } \
    } while (0)

#define CASE_ILLEGAL_OPTION \
    case TidyCharEncoding: \
    case TidyInCharEncoding: \
    case TidyOutCharEncoding: \
    case TidyErrFile: \
    case TidyOutFile: \
    case TidyWriteBack: \
    case TidyShowMarkup: \
    case TidyShowWarnings: \
    case TidyQuiet: \
    case TidyEmacs: \
    case TidyEmacsFile: \
    case TidyForceOutput: \
    case TidyShowErrors

#define EXTRACT_OPTION(opt_var, id_var, tdoc, term) \
    TidyOption opt_var; \
    TidyOptionId id_var; \
    do { \
        EXTRACT_STRING(name, term); \
        option = tidyGetOptionByName(tdoc, name); \
        enif_free(name); \
        if (opt_var == NULL) { \
            return enif_make_badarg(env); \
        } \
        id_var = tidyOptGetId(option); \
        switch (id_var) { \
            CASE_ILLEGAL_OPTION: \
                return enif_make_badarg(env); \
            default: \
                break; \
        } \
    } while (0)

#define EXTRACT_ATOM_INDEX(var, term, atoms) \
    ulong var; \
    do { \
        for (var = 0; var < sizeof(atoms) / sizeof(ERL_NIF_TERM); var++) { \
            if (enif_is_identical(atoms[var], term)) { \
                break; \
            } \
        } \
        if (var == sizeof(atoms) / sizeof(ERL_NIF_TERM)) { \
            return enif_make_badarg(env); \
        } \
    } while (0)

#define DRV_FUN(name) \
    ERL_NIF_TERM DRV_FUN_NAME(name)(ErlNifEnv *env, int argc, \
                                    const ERL_NIF_TERM argv[])

#define TDOC_FUN(name, code) \
    DRV_FUN(name) { \
        EXTRACT_TDOC(handle, tdoc, argv[0]); \
        code \
    }

#define TDOC_PROC(name, proc) \
    TDOC_FUN(name, { \
        proc(tdoc); \
        return enif_make_atom(env, "ok"); \
    })

#define UINT_PROPERTY_FUN(name, property_fun) \
    TDOC_FUN(name, { \
        return enif_make_uint(env, property_fun(tdoc)); \
    })

#define BOOL_PROPERTY_FUN(name, property_fun) \
    TDOC_FUN(name, { \
        return property_fun(tdoc) == yes \
               ? enif_make_atom(env, "true") \
               : enif_make_atom(env, "false"); \
    })

#define DRV_NIF_FUN(name, arity) \
    {#name, arity, DRV_FUN_NAME(name)}

static ErlNifEnv *enums_atoms_env = NULL;

static ERL_NIF_TERM attr_sort_strategies_atoms[2];
static ERL_NIF_TERM autobool_atoms[3];
static ERL_NIF_TERM doctype_modes_atoms[5];
static ERL_NIF_TERM duplicate_attrs_modes_atoms[2];
static ERL_NIF_TERM line_endings_atoms[3];
static ERL_NIF_TERM report_levels_atoms[7];

static ErlNifResourceType *tdoc_resource;

// Prototypes

static ERL_NIF_TERM tdoc_resource_new(ErlNifEnv *, TidyDoc);
static void tdoc_resource_cleanup(ErlNifEnv *, void *);
static void *tdoc_alloc(TidyAllocator *, size_t);
static void *tdoc_realloc(TidyAllocator *, void *, size_t);
static void tdoc_free(TidyAllocator *, void *);
static void tdoc_panic(TidyAllocator *, ctmbstr);
static Bool tdoc_report(TidyDoc, TidyReportLevel, uint, uint, ctmbstr);

static Bool tdoc_source_init(TidyInputSource *, tdoc_source_data *,
                             ErlNifEnv *, ERL_NIF_TERM);
static int tdoc_source_get_byte(void *);
static void tdoc_source_unget_byte(void *, byte);
static Bool tdoc_source_is_eof(void *);

DRV_FUN(release_date);
DRV_FUN(new);
DRV_FUN(status);
DRV_FUN(type);
DRV_FUN(errors_count);
DRV_FUN(warnings_count);
DRV_FUN(access_warnings_count);
DRV_FUN(config_errors_count);
DRV_FUN(encoding);
DRV_FUN(input_encoding);
DRV_FUN(output_encoding);
DRV_FUN(option);
DRV_FUN(option_is_readonly);
DRV_FUN(option_doc);
DRV_FUN(options);
DRV_FUN(parse);
DRV_FUN(clean_repair);
DRV_FUN(dump);
DRV_FUN(diagnostics);

static ErlNifFunc nif_funcs[] =
{
    DRV_NIF_FUN(release_date, 0),
    DRV_NIF_FUN(new, 0),
    DRV_NIF_FUN(new, 1),
    DRV_NIF_FUN(status, 1),
    DRV_NIF_FUN(type, 1),
    DRV_NIF_FUN(errors_count, 1),
    DRV_NIF_FUN(warnings_count, 1),
    DRV_NIF_FUN(access_warnings_count, 1),
    DRV_NIF_FUN(config_errors_count, 1),
    DRV_NIF_FUN(encoding, 2),
    DRV_NIF_FUN(input_encoding, 1),
    DRV_NIF_FUN(input_encoding, 2),
    DRV_NIF_FUN(output_encoding, 1),
    DRV_NIF_FUN(output_encoding, 2),
    DRV_NIF_FUN(option, 2),
    DRV_NIF_FUN(option, 3),
    DRV_NIF_FUN(option_is_readonly, 2),
    DRV_NIF_FUN(option_doc, 2),
    DRV_NIF_FUN(options, 1),
    DRV_NIF_FUN(options, 2),
    DRV_NIF_FUN(parse, 2),
    DRV_NIF_FUN(clean_repair, 1),
    DRV_NIF_FUN(dump, 1),
    DRV_NIF_FUN(diagnostics, 1)
};

DRV_FUN(release_date)
{
    return enif_make_string(env, tidyReleaseDate(), ERL_NIF_LATIN1);
}

DRV_FUN(new)
{
    switch (argc) {
        case 0:
            return tdoc_resource_new(env, NULL);
        case 1: {
            EXTRACT_TDOC(handle, base, argv[0]);
            return tdoc_resource_new(env, base);
        }
    }
    return enif_make_badarg(env);
}

UINT_PROPERTY_FUN(status, tidyStatus);

TDOC_FUN(type,
{
    int html_version = tidyDetectedHtmlVersion(tdoc);
    if (html_version) {
        return enif_make_tuple2(env,
                enif_make_atom(env, "html"), enif_make_int(env, html_version));
    }

    if (tidyDetectedXhtml(tdoc) == yes) {
        return enif_make_atom(env, "xhtml");
    }

    if (tidyDetectedGenericXml(tdoc) == yes) {
        return enif_make_atom(env, "xml");
    }

    return enif_make_atom(env, "undefined");
});

UINT_PROPERTY_FUN(errors_count, tidyErrorCount);
UINT_PROPERTY_FUN(warnings_count, tidyWarningCount);
UINT_PROPERTY_FUN(access_warnings_count, tidyAccessWarningCount);
UINT_PROPERTY_FUN(config_errors_count, tidyConfigErrorCount);

TDOC_FUN(encoding,
{
    EXTRACT_STRING(enc, argv[1]);
    int res = tidySetCharEncoding(tdoc, enc);
    enif_free(enc);
    if (res == 0) {
        return enif_make_atom(env, "ok");
    }
    return enif_make_badarg(env);
});

#define ENCODING_FUN(name, option, set_fun) \
    TDOC_FUN(name, { \
        switch (argc) { \
            case 1: \
                return enif_make_string(env, \
                        tidyOptGetEncName(tdoc, option), ERL_NIF_LATIN1); \
            case 2: { \
                EXTRACT_STRING(enc, argv[1]); \
                int res = set_fun(tdoc, enc); \
                enif_free(enc); \
                if (res == 0) { \
                    return enif_make_atom(env, "ok"); \
                } \
                break; \
            } \
        } \
        return enif_make_badarg(env); \
    })

ENCODING_FUN(input_encoding, TidyInCharEncoding, tidySetInCharEncoding);
ENCODING_FUN(output_encoding, TidyOutCharEncoding, tidySetOutCharEncoding);

#define CASE_TAGS_LIST_OPTION \
    case TidyInlineTags: \
    case TidyBlockTags: \
    case TidyEmptyTags: \
    case TidyPreTags

#if SUPPORT_UTF16_ENCODINGS
#define CASE_OUTPUT_BOM_ \
    case TidyOutputBOM:
#else
#define CASE_OUTPUT_BOM_
#endif

#define CASE_AUTOBOOL_OPTION \
    case TidyIndentContent: \
    case TidyBodyOnly: \
    CASE_OUTPUT_BOM_ \
    case TidyMergeDivs: \
    case TidyMergeSpans

static ERL_NIF_TERM tdoc_get_option(ErlNifEnv *env, TidyDoc tdoc,
                                    TidyOptionId id, TidyOptionType type)
{
    switch (type) {
        case TidyString:
            switch (id) {
                CASE_TAGS_LIST_OPTION:
                    // TODO
                default: {
                    ctmbstr value = tidyOptGetValue(tdoc, id);
                    return value == NULL
                           ? enif_make_atom(env, "undefined")
                           : enif_make_string(env, value, ERL_NIF_LATIN1);
                }
            }
        case TidyInteger: {
            ulong value = tidyOptGetInt(tdoc, id);
            switch (id) {
                case TidyNewline:
                    return enif_make_copy(env,
                            line_endings_atoms[value - (ulong)TidyLF]);
                case TidyDoctypeMode:
                    return enif_make_copy(env,
                            doctype_modes_atoms[value - (ulong)TidyDoctypeOmit]);
                case TidyDuplicateAttrs:
                    return enif_make_copy(env,
                            duplicate_attrs_modes_atoms[value - (ulong)TidyKeepFirst]);
                CASE_AUTOBOOL_OPTION:
                    return enif_make_copy(env,
                            autobool_atoms[value - (ulong)TidyNoState]);
                case TidySortAttributes:
                    return enif_make_copy(env,
                            attr_sort_strategies_atoms[value - (ulong)TidySortAttrNone]);
                default:
                    return enif_make_ulong(env, value);
            }
        }
        case TidyBoolean:
            return tidyOptGetBool(tdoc, id) == yes
                   ? enif_make_atom(env, "true")
                   : enif_make_atom(env, "false");
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM tdoc_set_option(ErlNifEnv *env, TidyDoc tdoc,
                                    TidyOptionId id, TidyOptionType type,
                                    ERL_NIF_TERM term)
{
    switch (type) {
        case TidyString:
            switch (id) {
                CASE_TAGS_LIST_OPTION:
                    // TODO
                default: {
                    EXTRACT_STRING_OR_UNDEFINED(value, term);
                    Bool res = tidyOptSetValue(tdoc, id, value);
                    enif_free(value);
                    if (res == no) {
                        return enif_make_badarg(env);
                    }
                }
            }
            break;
        case TidyInteger: {
            ulong value;
            switch (id) {
                case TidyNewline: {
                    EXTRACT_ATOM_INDEX(index, term, line_endings_atoms);
                    value = index + (ulong)TidyLF;
                    break;
                }
                case TidyDoctypeMode: {
                    EXTRACT_ATOM_INDEX(index, term, doctype_modes_atoms);
                    value = index + (ulong)TidyDoctypeOmit;
                    break;
                }
                case TidyDuplicateAttrs: {
                    EXTRACT_ATOM_INDEX(
                            index, term, duplicate_attrs_modes_atoms);
                    value = index + (ulong)TidyKeepFirst;
                    break;
                }
                CASE_AUTOBOOL_OPTION: {
                    EXTRACT_ATOM_INDEX(index, term, autobool_atoms);
                    value = index + (ulong)TidyNoState;
                    break;
                }
                case TidySortAttributes: {
                    EXTRACT_ATOM_INDEX(
                            index, term, attr_sort_strategies_atoms);
                    value = index + (ulong)TidySortAttrNone;
                    break;
                }
                default:
                    if (!enif_get_ulong(env, term, &value)) {
                        return enif_make_badarg(env);
                    }
                    break;
            }
            if (tidyOptSetInt(tdoc, id, value) == no) {
                return enif_make_badarg(env);
            }
            break;
        }
        case TidyBoolean: {
            Bool value;
            if (enif_is_identical(term, enif_make_atom(env, "false"))) {
                value = no;
            } else if (enif_is_identical(term, enif_make_atom(env, "true"))) {
                value = yes;
            } else {
                return enif_make_badarg(env);
            }
            if (tidyOptSetBool(tdoc, id, value) == no) {
                return enif_make_badarg(env);
            }
            break;
        }
    }
    return enif_make_atom(env, "ok");
}

TDOC_FUN(option,
{
    EXTRACT_OPTION(option, id, tdoc, argv[1]);
    TidyOptionType type = tidyOptGetType(option);

    switch (argc) {
        case 2:
            return tdoc_get_option(env, tdoc, id, type);
        case 3:
            return tdoc_set_option(env, tdoc, id, type, argv[2]);
    }

    return enif_make_badarg(env);
});

TDOC_FUN(option_is_readonly,
{
    EXTRACT_OPTION(option, id, tdoc, argv[1]);
    return tidyOptIsReadOnly(option) == yes
           ? enif_make_atom(env, "true")
           : enif_make_atom(env, "false");
});

TDOC_FUN(option_doc,
{
    EXTRACT_OPTION(option, id, tdoc, argv[1]);
    ctmbstr doc = tidyOptGetDoc(tdoc, option);
    return doc == NULL
           ? enif_make_atom(env, "undefined")
           : enif_make_string(env, doc, ERL_NIF_LATIN1);
});

TDOC_FUN(options,
{
    switch (argc) {
        case 1: {
            TidyIterator it = tidyGetOptionList(tdoc);
            TidyBuffer buf;
            TidyOption option;
            tidyBufInitWithAllocator(&buf, &(handle->allocator));
            while ((option = tidyGetNextOption(tdoc, &it)) != NULL) {
                TidyOptionId id = tidyOptGetId(option);
                switch (id) {
                    CASE_ILLEGAL_OPTION:
                        continue;
                    default: {
                        TidyOptionType type = tidyOptGetType(option);
                        ERL_NIF_TERM name = enif_make_string(
                                env, tidyOptGetName(option), ERL_NIF_LATIN1);
                        ERL_NIF_TERM value = tdoc_get_option(
                                env, tdoc, id, type);
                        ERL_NIF_TERM tuple = enif_make_tuple2(
                                env, name, value);
                        tidyBufAppend(&buf, &tuple, sizeof(ERL_NIF_TERM));
                    }
                }
            }
            ERL_NIF_TERM result = enif_make_list_from_array(
                    env,
                    (ERL_NIF_TERM *)buf.bp, buf.size / sizeof(ERL_NIF_TERM));
            tidyBufFree(&buf);
            return result;
        }
        case 2: {
            if (!enif_is_list(env, argv[1])) {
                return enif_make_badarg(env);
            }

            ERL_NIF_TERM tuple;
            ERL_NIF_TERM list = argv[1];
            while (enif_get_list_cell(env, list, &tuple, &list)) {
                const ERL_NIF_TERM *elements;
                int arity;
                if (!enif_get_tuple(env, tuple, &arity, &elements)) {
                    return enif_make_badarg(env);
                }
                if (arity != 2) {
                    return enif_make_badarg(env);
                }
                EXTRACT_OPTION(option, id, tdoc, elements[0]);
                TidyOptionType type = tidyOptGetType(option);
                ERL_NIF_TERM res = tdoc_set_option(
                        env, tdoc, id, type, elements[1]);
                if (!enif_is_atom(env, res)) {
                    return res;
                }
            }
            return enif_make_atom(env, "ok");
        }
    }

    return enif_make_badarg(env);
});

TDOC_FUN(parse,
{
    TidyInputSource source;
    tdoc_source_data data;
    if (tdoc_source_init(&source, &data, env, argv[1]) == no) {
        return enif_make_badarg(env);
    }
    TidyBuffer *buf = tidyGetAppData(tdoc);
    if (buf) {
        tidyBufClear(buf);
    } else {
        buf = enif_alloc(sizeof(TidyBuffer));
        tidyBufInit(buf);
        tidySetAppData(tdoc, buf);
    }
    int status = tidyParseSource(tdoc, &source);
    return enif_make_int(env, status);
});

TDOC_FUN(clean_repair,
{
    return enif_make_int(env, tidyCleanAndRepair(tdoc));
});

TDOC_FUN(dump,
{
    TidyBuffer buf;
    tidyBufInitWithAllocator(&buf, &(handle->allocator));
    tidySaveBuffer(tdoc, &buf);
    ErlNifBinary bin;
    enif_alloc_binary(buf.size, &bin);
    memcpy(bin.data, buf.bp, bin.size);
    tidyBufFree(&buf);
    return enif_make_binary(env, &bin);
});

TDOC_FUN(diagnostics,
{
    TidyBuffer *buf = tidyGetAppData(tdoc);
    ERL_NIF_TERM list = enif_make_list(env, 0);
    if (!buf || !buf->size) {
        return list;
    }

    size_t i;
    tdoc_report_entry *reports = (tdoc_report_entry *)buf->bp;
    for (i = buf->size / sizeof(tdoc_report_entry); i--;) {
        ERL_NIF_TERM lvl = enif_make_copy(
                env,
                report_levels_atoms[(ulong)reports[i].lvl - (ulong)TidyInfo]);
        ERL_NIF_TERM line = enif_make_uint(env, reports[i].line);
        ERL_NIF_TERM col = enif_make_uint(env, reports[i].col);
        ERL_NIF_TERM msg = enif_make_string(
                env, reports[i].msg, ERL_NIF_LATIN1);
        list = enif_make_list_cell(
                env, enif_make_tuple4(env, lvl, line, col, msg), list);
    }
    return list;
});

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    if (enums_atoms_env == NULL) {
        enums_atoms_env = enif_alloc_env();

        attr_sort_strategies_atoms[0] = enif_make_atom(
                enums_atoms_env, "none");
        attr_sort_strategies_atoms[1] = enif_make_atom(
                enums_atoms_env, "alpha");

        autobool_atoms[0] = enif_make_atom(enums_atoms_env, "false");
        autobool_atoms[1] = enif_make_atom(enums_atoms_env, "true");
        autobool_atoms[2] = enif_make_atom(enums_atoms_env, "auto");

        doctype_modes_atoms[0] = enif_make_atom(enums_atoms_env, "omit");
        doctype_modes_atoms[1] = enif_make_atom(enums_atoms_env, "auto");
        doctype_modes_atoms[2] = enif_make_atom(enums_atoms_env, "strict");
        doctype_modes_atoms[3] = enif_make_atom(enums_atoms_env, "loose");
        doctype_modes_atoms[4] = enif_make_atom(enums_atoms_env, "user");

        duplicate_attrs_modes_atoms[0] = enif_make_atom(
                enums_atoms_env, "keep_first");
        duplicate_attrs_modes_atoms[1] = enif_make_atom(
                enums_atoms_env, "keep_last");

        line_endings_atoms[0] = enif_make_atom(enums_atoms_env, "lf");
        line_endings_atoms[1] = enif_make_atom(enums_atoms_env, "crlf");
        line_endings_atoms[2] = enif_make_atom(enums_atoms_env, "cr");

        report_levels_atoms[0] = enif_make_atom(enums_atoms_env, "info");
        report_levels_atoms[1] = enif_make_atom(enums_atoms_env, "warning");
        report_levels_atoms[2] = enif_make_atom(enums_atoms_env, "config");
        report_levels_atoms[3] = enif_make_atom(enums_atoms_env, "access");
        report_levels_atoms[4] = enif_make_atom(enums_atoms_env, "error");
        report_levels_atoms[5] = enif_make_atom(
                enums_atoms_env, "bad_document");
        report_levels_atoms[6] = enif_make_atom(enums_atoms_env, "fatal");
    }

    tdoc_resource = enif_open_resource_type(
            env, NULL, "ex_tidy_drv_tdoc_resource", tdoc_resource_cleanup,
            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, 0);

    return 0;
}

ERL_NIF_INIT(ex_tidy_drv, nif_funcs, &on_load, NULL, NULL, NULL);

static ERL_NIF_TERM tdoc_resource_new(ErlNifEnv *env, TidyDoc base)
{
    tdoc_handle *handle = enif_alloc_resource(
            tdoc_resource, sizeof(tdoc_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    handle->vtbl.alloc = tdoc_alloc;
    handle->vtbl.realloc = tdoc_realloc;
    handle->vtbl.free = tdoc_free;
    handle->vtbl.panic = tdoc_panic;
    handle->allocator.vtbl = &(handle->vtbl);
    handle->tdoc = tidyCreateWithAllocator(&(handle->allocator));
    if (base != NULL) {
        tidyOptCopyConfig(handle->tdoc, base);
    } else {
        tidyOptSetBool(handle->tdoc, TidyQuiet, yes);
    }
    tidySetReportFilter(handle->tdoc, tdoc_report);
    enif_release_resource(handle);
    return result;
}

static void tdoc_resource_cleanup(ErlNifEnv *env, void *arg)
{
    tidyRelease(((tdoc_handle *)arg)->tdoc);
}

static void *tdoc_alloc(TidyAllocator *allocator, size_t size)
{
    return enif_alloc(size);
}

static void *tdoc_realloc(TidyAllocator *allocator, void *block, size_t size)
{
    return enif_realloc(block, size);
}

static void tdoc_free(TidyAllocator *allocator, void *block)
{
    enif_free(block);
}

static void tdoc_panic(TidyAllocator *allocator, ctmbstr msg)
{
    exit(ENOMEM); // TODO longjmp
}

static Bool tdoc_report(TidyDoc tdoc, TidyReportLevel lvl, uint line, uint col,
                        ctmbstr msg)
{
    TidyBuffer *buf = tidyGetAppData(tdoc);
    size_t len = strlen(msg) + 1;
    tmbstr report_msg = enif_alloc(len);
    strncpy(report_msg, msg, len);
    tdoc_report_entry report = {lvl, line, col, report_msg};
    tidyBufAppend(buf, &report, sizeof(tdoc_report_entry));
    return no;
}

static Bool tdoc_source_init(TidyInputSource *source, tdoc_source_data *data,
                             ErlNifEnv *env, ERL_NIF_TERM term)
{
    if (!enif_inspect_binary(env, term, &(data->bin))) {
        return no;
    }
    data->pos = 0;
    return tidyInitSource(source, data, tdoc_source_get_byte,
                          tdoc_source_unget_byte, tdoc_source_is_eof);
}

static int tdoc_source_get_byte(void *data)
{
    tdoc_source_data *src_data = (tdoc_source_data *)data;
    return src_data->bin.data[src_data->pos++];
}

static void tdoc_source_unget_byte(void *data, byte ch)
{
    --((tdoc_source_data *)data)->pos;
}

static Bool tdoc_source_is_eof(void *data)
{
    tdoc_source_data *src_data = (tdoc_source_data *)data;
    return src_data->bin.size == src_data->pos ? yes : no;
}
