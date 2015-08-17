#ifndef IDS_H
#define IDS_H

#include <wx/wx.h>

// some ids must be the same as wxids because where are going to redirect this 
// events and we want default wx handlers to process it (see mxMainWindow::OnEditNeedFocus)
#define mxID_EDIT_COPY wxID_COPY
#define mxID_EDIT_CUT wxID_CUT
#define mxID_EDIT_PASTE wxID_PASTE

enum {
	mxID_MAIN = wxID_HIGHEST+1,
	mxID_NOTEBOOK_SOURCES,
	mxID_NOTEBOOK_INSPECTIONS,
	mxID_LEFT_PANELS,
	
	mxID_FILE_NEW,
	mxID_FILE_OPEN,
	mxID_FILE_SAVE,
	mxID_FILE_SAVE_AS,
	mxID_FILE_CLOSE,
	mxID_FILE_EXIT,
	mxID_FILE_SOURCE_RECENT,
	mxID_FILE_PROJECT_RECENT,
	mxID_FILE_PROPERTIES,
	mxID_FILE_OPEN_FOLDER,
	mxID_FILE_EXPLORE_FOLDER,
	mxID_FILE_PRINT,
	mxID_FILE_OPEN_SELECTED,
	mxID_FILE_PROJECT,
	mxID_FILE_PREFERENCES,
	mxID_FILE_PROJECT_CONFIG,
	mxID_FILE_SHARE,
	mxID_FILE_SAVE_ALL,
	mxID_FILE_SAVE_PROJECT,
	mxID_FILE_CLOSE_PROJECT,
	mxID_FILE_CLOSE_ALL,
	mxID_FILE_CLOSE_ALL_BUT_ONE,
	mxID_FILE_OPEN_H,
	mxID_FILE_RELOAD,
	mxID_FILE_EXPORT_HTML,
	mxID_FILE_SOURCE_HISTORY_0,
	mxID_FILE_SOURCE_HISTORY_1,
	mxID_FILE_SOURCE_HISTORY_2,
	mxID_FILE_SOURCE_HISTORY_3,
	mxID_FILE_SOURCE_HISTORY_4,
	mxID_FILE_SOURCE_HISTORY_5,
	mxID_FILE_SOURCE_HISTORY_6,
	mxID_FILE_SOURCE_HISTORY_7,
	mxID_FILE_SOURCE_HISTORY_8,
	mxID_FILE_SOURCE_HISTORY_9,
	mxID_FILE_SOURCE_HISTORY_10,
	mxID_FILE_SOURCE_HISTORY_11,
	mxID_FILE_SOURCE_HISTORY_12,
	mxID_FILE_SOURCE_HISTORY_13,
	mxID_FILE_SOURCE_HISTORY_14,
	mxID_FILE_SOURCE_HISTORY_15,
	mxID_FILE_SOURCE_HISTORY_16,
	mxID_FILE_SOURCE_HISTORY_17,
	mxID_FILE_SOURCE_HISTORY_18,
	mxID_FILE_SOURCE_HISTORY_19,
	mxID_FILE_SOURCE_HISTORY_20,
	mxID_FILE_SOURCE_HISTORY_21,
	mxID_FILE_SOURCE_HISTORY_22,
	mxID_FILE_SOURCE_HISTORY_23,
	mxID_FILE_SOURCE_HISTORY_24,
	mxID_FILE_SOURCE_HISTORY_25,
	mxID_FILE_SOURCE_HISTORY_26,
	mxID_FILE_SOURCE_HISTORY_27,
	mxID_FILE_SOURCE_HISTORY_28,
	mxID_FILE_SOURCE_HISTORY_29,
	mxID_FILE_SOURCE_HISTORY_30,
	mxID_FILE_SOURCE_HISTORY_31,
	mxID_FILE_SOURCE_HISTORY_32,
	mxID_FILE_SOURCE_HISTORY_33,
	mxID_FILE_SOURCE_HISTORY_34,
	mxID_FILE_SOURCE_HISTORY_35,
	mxID_FILE_SOURCE_HISTORY_36,
	mxID_FILE_SOURCE_HISTORY_37,
	mxID_FILE_SOURCE_HISTORY_38,
	mxID_FILE_SOURCE_HISTORY_39,
	mxID_FILE_SOURCE_HISTORY_40,
	mxID_FILE_SOURCE_HISTORY_41,
	mxID_FILE_SOURCE_HISTORY_42,
	mxID_FILE_SOURCE_HISTORY_43,
	mxID_FILE_SOURCE_HISTORY_44,
	mxID_FILE_SOURCE_HISTORY_45,
	mxID_FILE_SOURCE_HISTORY_46,
	mxID_FILE_SOURCE_HISTORY_47,
	mxID_FILE_SOURCE_HISTORY_48,
	mxID_FILE_SOURCE_HISTORY_49,
	mxID_FILE_SOURCE_HISTORY_50,
	mxID_FILE_SOURCE_HISTORY_CLEAR,
	mxID_FILE_SOURCE_HISTORY_MORE,
	mxID_FILE_PROJECT_HISTORY_0,
	mxID_FILE_PROJECT_HISTORY_1,
	mxID_FILE_PROJECT_HISTORY_2,
	mxID_FILE_PROJECT_HISTORY_3,
	mxID_FILE_PROJECT_HISTORY_4,
	mxID_FILE_PROJECT_HISTORY_5,
	mxID_FILE_PROJECT_HISTORY_6,
	mxID_FILE_PROJECT_HISTORY_7,
	mxID_FILE_PROJECT_HISTORY_8,
	mxID_FILE_PROJECT_HISTORY_9,
	mxID_FILE_PROJECT_HISTORY_10,
	mxID_FILE_PROJECT_HISTORY_11,
	mxID_FILE_PROJECT_HISTORY_12,
	mxID_FILE_PROJECT_HISTORY_13,
	mxID_FILE_PROJECT_HISTORY_14,
	mxID_FILE_PROJECT_HISTORY_15,
	mxID_FILE_PROJECT_HISTORY_16,
	mxID_FILE_PROJECT_HISTORY_17,
	mxID_FILE_PROJECT_HISTORY_18,
	mxID_FILE_PROJECT_HISTORY_19,
	mxID_FILE_PROJECT_HISTORY_20,
	mxID_FILE_PROJECT_HISTORY_21,
	mxID_FILE_PROJECT_HISTORY_22,
	mxID_FILE_PROJECT_HISTORY_23,
	mxID_FILE_PROJECT_HISTORY_24,
	mxID_FILE_PROJECT_HISTORY_25,
	mxID_FILE_PROJECT_HISTORY_26,
	mxID_FILE_PROJECT_HISTORY_27,
	mxID_FILE_PROJECT_HISTORY_28,
	mxID_FILE_PROJECT_HISTORY_29,
	mxID_FILE_PROJECT_HISTORY_30,
	mxID_FILE_PROJECT_HISTORY_31,
	mxID_FILE_PROJECT_HISTORY_32,
	mxID_FILE_PROJECT_HISTORY_33,
	mxID_FILE_PROJECT_HISTORY_34,
	mxID_FILE_PROJECT_HISTORY_35,
	mxID_FILE_PROJECT_HISTORY_36,
	mxID_FILE_PROJECT_HISTORY_37,
	mxID_FILE_PROJECT_HISTORY_38,
	mxID_FILE_PROJECT_HISTORY_39,
	mxID_FILE_PROJECT_HISTORY_40,
	mxID_FILE_PROJECT_HISTORY_41,
	mxID_FILE_PROJECT_HISTORY_42,
	mxID_FILE_PROJECT_HISTORY_43,
	mxID_FILE_PROJECT_HISTORY_44,
	mxID_FILE_PROJECT_HISTORY_45,
	mxID_FILE_PROJECT_HISTORY_46,
	mxID_FILE_PROJECT_HISTORY_47,
	mxID_FILE_PROJECT_HISTORY_48,
	mxID_FILE_PROJECT_HISTORY_49,
	mxID_FILE_PROJECT_HISTORY_50,
	mxID_FILE_PROJECT_HISTORY_CLEAR,
	mxID_FILE_PROJECT_HISTORY_MORE,
	mxID_FILE_SET_AS_MASTER,
	mxID_EDIT_MAKE_UPPERCASE,
	mxID_EDIT_MAKE_LOWERCASE,
	mxID_EDIT_SELECT_ALL,
	mxID_EDIT_UNDO,
	mxID_EDIT_REDO,
//	mxID_EDIT_COPY,
//	mxID_EDIT_CUT,
//	mxID_EDIT_PASTE,
	mxID_EDIT_BRACEMATCH,
	mxID_EDIT_GOTO,
	mxID_EDIT_GOTO_FUNCTION,
	mxID_EDIT_GOTO_FILE,
	mxID_SOURCE_GOTO_DEFINITION,
	mxID_EDIT_FIND,
	mxID_EDIT_FIND_FROM_TOOLBAR,
	mxID_EDIT_TOOLBAR_FIND,
	mxID_EDIT_FIND_NEXT,
	mxID_EDIT_FIND_PREV,
	mxID_EDIT_REPLACE,
	mxID_EDIT_FIND_KEYWORD,
	mxID_EDIT_MARK_LINES,
	mxID_EDIT_GOTO_MARK,
	mxID_EDIT_LIST_MARKS,
	mxID_EDIT_INDENT,
	mxID_EDIT_COMMENT,
	mxID_EDIT_UNCOMMENT,
	mxID_EDIT_DELETE_LINES,
	mxID_EDIT_DUPLICATE_LINES,
	mxID_EDIT_INSERT_HEADER,
	mxID_EDIT_TOGGLE_LINES_UP,
	mxID_EDIT_TOGGLE_LINES_DOWN,
	mxID_EDIT_RECTANGULAR_EDITION,
	mxID_EDIT_FORCE_AUTOCOMPLETE,
	mxID_EDIT_AUTOCODE_AUTOCOMPLETE,
	mxID_EDIT_FUZZY_AUTOCOMPLETE,
	mxID_EDIT_HIGHLIGHT_WORD,
	mxID_VIEW_BEGINNER_PANEL,
	mxID_VIEW_NEXT_ERROR,
	mxID_VIEW_PREV_ERROR,
	mxID_VIEW_LEFT_PANELS,
	mxID_VIEW_LINE_WRAP,
	mxID_VIEW_WHITE_SPACE,
	mxID_VIEW_CODE_STYLE,
	mxID_VIEW_CODE_COLOURS,
	mxID_VIEW_PROJECT_TREE,
	mxID_VIEW_EXPLORER_TREE,
	mxID_VIEW_COMPILER_TREE,
	mxID_VIEW_SYMBOLS_TREE,
	mxID_VIEW_UPDATE_SYMBOLS,
	mxID_VIEW_TOOLBAR_VIEW,
	mxID_VIEW_TOOLBAR_FILE,
	mxID_VIEW_TOOLBAR_TOOLS,
	mxID_VIEW_TOOLBAR_PROJECT,
	mxID_VIEW_TOOLBAR_DEBUG,
	mxID_VIEW_TOOLBAR_EDIT,
	mxID_VIEW_TOOLBAR_RUN,
	mxID_VIEW_TOOLBAR_MISC,
	mxID_VIEW_TOOLBAR_FIND,
	mxID_VIEW_TOOLBARS_CONFIG,
	mxID_VIEW_NOTEBOOK_NEXT,
	mxID_VIEW_NOTEBOOK_PREV,
	mxID_VIEW_HIDE_BOTTOM,
	mxID_VIEW_FULLSCREEN,
	mxID_FOLD_SHOW_1,
	mxID_FOLD_SHOW_2,
	mxID_FOLD_SHOW_3,
	mxID_FOLD_SHOW_4,
	mxID_FOLD_SHOW_5,
	mxID_FOLD_SHOW_ALL,
	mxID_FOLD_HIDE_1,
	mxID_FOLD_HIDE_2,
	mxID_FOLD_HIDE_3,
	mxID_FOLD_HIDE_4,
	mxID_FOLD_HIDE_5,
	mxID_FOLD_HIDE_ALL,
	mxID_FOLD_FOLD,
	mxID_FOLD_UNFOLD,
	mxID_VIEW_HIDE_SOMETHING,
	mxID_VIEW_DUPLICATE_TAB,

	mxID_NAVIGATION_HISTORY_PREV,
	mxID_NAVIGATION_HISTORY_NEXT,
	mxID_WHERE_AM_I,
		
	mxID_RUN_RUN,
	mxID_RUN_RUN_OLD,
	mxID_RUN_STOP,
	mxID_RUN_COMPILE,
//	mxID_RUN_BUILD,
	mxID_RUN_CONFIG,
	mxID_RUN_CLEAN,

	mxID_DEBUG_ATTACH,
	mxID_DEBUG_TARGET,
	mxID_DEBUG_JUMP,
	mxID_DEBUG_RUN,
	mxID_DEBUG_PAUSE,
	mxID_DEBUG_CONTINUE,
	mxID_DEBUG_STOP,
	mxID_DEBUG_STEP_IN,
	mxID_DEBUG_STEP_OVER,
	mxID_DEBUG_STEP_OUT,
	mxID_DEBUG_INSPECT,
	mxID_DEBUG_UPDATE_INSPECTIONS,
	mxID_DEBUG_BACKTRACE,
	mxID_DEBUG_THREADLIST,
	mxID_DEBUG_FUNCTION_BREAKPOINT,
	mxID_DEBUG_LIST_BREAKPOINTS,
	mxID_DEBUG_TOGGLE_BREAKPOINT,
	mxID_DEBUG_ENABLE_DISABLE_BREAKPOINT,
	mxID_DEBUG_BREAKPOINT_OPTIONS,
	mxID_DEBUG_INSERT_WATCHPOINT,
	mxID_DEBUG_LIST_WATCHPOINTS,
	mxID_DEBUG_RUN_UNTIL,
	mxID_DEBUG_RETURN,
	mxID_DEBUG_DO_THAT,
	mxID_DEBUG_PATCH,
	mxID_DEBUG_SAVE_CORE_DUMP,
	mxID_DEBUG_LOAD_CORE_DUMP,
	mxID_DEBUG_ENABLE_INVERSE_EXEC,
	mxID_DEBUG_INVERSE_EXEC,
	mxID_DEBUG_LOG_PANEL,
	mxID_DEBUG_SET_SIGNALS,
	mxID_DEBUG_SEND_SIGNAL,
	mxID_DEBUG_GDB_COMMAND,
	mxID_INTERNAL_INFO,

	mxID_TOOLS_MAKEFILE,
	mxID_TOOLS_CREATE_TEMPLATE,
	mxID_TOOLS_PREPROC_MARK_VALID,
	mxID_TOOLS_PREPROC_UNMARK_ALL,
	mxID_TOOLS_PREPROC_EXPAND_MACROS,
	mxID_TOOLS_PREPROC_HELP,
	mxID_TOOLS_CODE_COPY_FROM_H,
	mxID_TOOLS_DRAW_FLOW,
	mxID_TOOLS_DRAW_CLASSES,
	mxID_TOOLS_DRAW_PROJECT,
	mxID_TOOLS_SOURCE_GRAPHER,
	mxID_TOOLS_PROJECT_STATISTICS,
	mxID_TOOLS_EXE_PROPS,
	mxID_TOOLS_SHARE_HELP,
	mxID_TOOLS_SHARE_LIST,
	mxID_TOOLS_SHARE_SHARE,
	mxID_TOOLS_SHARE_OPEN,
	mxID_TOOLS_DIFF_TWO,
	mxID_TOOLS_DIFF_DISK,
	mxID_TOOLS_DIFF_HIMSELF,
	mxID_TOOLS_DIFF_CLEAR,
	mxID_TOOLS_DIFF_HELP,
	mxID_TOOLS_DIFF_APPLY,
	mxID_TOOLS_DIFF_SHOW,
	mxID_TOOLS_DIFF_DISCARD,
	mxID_TOOLS_DIFF_NEXT,
	mxID_TOOLS_DIFF_PREV,
	mxID_TOOLS_DOXY_HELP,
	mxID_TOOLS_DOXY_CONFIG,
	mxID_TOOLS_DOXY_GENERATE,
	mxID_TOOLS_DOXY_VIEW,
	mxID_TOOLS_REMOVE_COMMENTS,
	mxID_TOOLS_ALIGN_COMMENTS,
	mxID_TOOLS_INSTALL_COMPLEMENTS,
		
	mxID_TOOLS_WXFB_UPDATE_INHERIT,
	mxID_TOOLS_WXFB_INHERIT_CLASS,
	mxID_TOOLS_WXFB_CONFIG,
	mxID_TOOLS_WXFB_NEW_RES,
	mxID_TOOLS_WXFB_LOAD_RES,
	mxID_TOOLS_WXFB_REGEN,
	mxID_TOOLS_WXFB_HELP,
	mxID_TOOLS_WXFB_HELP_WX,
		
	mxID_TOOLS_LIZARD_RUN,
	mxID_TOOLS_LIZARD_HELP,

	mxID_TOOLS_GPROF_SET,
	mxID_TOOLS_GPROF_SHOW,
	mxID_TOOLS_GPROF_LIST,
	mxID_TOOLS_GPROF_HELP,
	mxID_TOOLS_GPROF_DOT,
	mxID_TOOLS_GPROF_FDP,
		
	mxID_TOOLS_GCOV_SET,
	mxID_TOOLS_GCOV_RESET,
	mxID_TOOLS_GCOV_SHOW,
	mxID_TOOLS_GCOV_HELP,
	mxID_GCOV_REFRESH,
	
	mxID_TOOLS_VALGRIND_RUN,
	mxID_TOOLS_VALGRIND_DEBUG,
	mxID_TOOLS_VALGRIND_VIEW,
	mxID_TOOLS_VALGRIND_HELP,
	
	mxID_TOOLS_CPPCHECK_RUN,
	mxID_TOOLS_CPPCHECK_CONFIG,
	mxID_TOOLS_CPPCHECK_VIEW,
	mxID_TOOLS_CPPCHECK_HELP,
	
	mxID_TOOLS_CONSOLE,
		
	mxID_TOOLS_CUSTOM_TOOLS,
	mxID_TOOLS_CUSTOM_TOOLS_SETTINGS,
	mxID_TOOLS_PROJECT_TOOLS_SETTINGS,
	mxID_TOOLS_CUSTOM_HELP,
		
	mxID_CHANGE_SHORTCUTS,
	mxID_MACRO_RECORD,
	mxID_MACRO_REPLAY,
		
	mxID_COMPLEMENTS_PATH,
		
	mxID_AUTOCODE_DECLARE_VAR,
	mxID_AUTOCODE_DECLARE_ARRAY,
	mxID_AUTOCODE_DECLARE_MATRIX,
	mxID_AUTOCODE_IF_THEN,
	mxID_AUTOCODE_IF_THEN_ELSE,
	mxID_AUTOCODE_FOR,
	mxID_AUTOCODE_WHILE,
	mxID_AUTOCODE_DO_WHILE,
	mxID_AUTOCODE_SWITCH,
	mxID_AUTOCODE_ASSIGN,
	mxID_AUTOCODE_COUT,
	mxID_AUTOCODE_CIN,
	mxID_AUTOCODE_STRUCT,
	mxID_AUTOCODE_CLASS,
	mxID_AUTOCODE_INHERIT,
	mxID_AUTOCODE_ATTRIBUTE,
	mxID_AUTOCODE_MEMBER,
	
	mxID_HELP_ABOUT,
	mxID_HELP_CPP,
	mxID_HELP_CODE,
	mxID_HELP_TUTORIAL,
	mxID_HELP_GUI,
	mxID_HELP_TIP,
	mxID_HELP_UPDATES,
	mxID_HELP_OPINION,
	mxID_HELP_SHORTCUTS,

	mxID_FOLDTOGGLE,
	
	mxID_DOUBLE_CLICK_PROJECT_TREE,
	mxID_DOUBLE_CLICK_COMPILER_TREE,
	mxID_TREE_EXPLORER,
	mxID_EXPLORER_POPUP_UPDATE,
	mxID_EXPLORER_POPUP_CHANGE_PATH,
	mxID_EXPLORER_POPUP_PATH_UP,
	mxID_EXPLORER_POPUP_OPEN_ONE,
	mxID_EXPLORER_POPUP_OPEN_ALL,
	mxID_EXPLORER_POPUP_OPEN_SOURCES,
	mxID_EXPLORER_POPUP_SHOW_ONLY_SOURCES,
	mxID_EXPLORER_POPUP_SET_AS_PATH,
	mxID_TREE_PROJECT,
	mxID_TREE_COMPILER,
	mxID_COMPILER_POPUP_FULL,
	mxID_TREE_SYMBOLS,
	mxID_SYMBOL_POPUP_INCLUDES,
	mxID_SYMBOL_POPUP_DEC,
 	mxID_SYMBOL_POPUP_DEF,
 	mxID_SYMBOL_GENERATE_CACHE,

	mxID_PROJECT_POPUP_READONLY,
	mxID_PROJECT_POPUP_HIDE_SYMBOLS,
	mxID_PROJECT_POPUP_TOGGLE_FULLPATH,
	mxID_PROJECT_POPUP_OPEN_FOLDER,
	mxID_PROJECT_POPUP_PROPERTIES,
	mxID_PROJECT_POPUP_OPEN,
	mxID_PROJECT_POPUP_OPEN_ALL,
	mxID_PROJECT_POPUP_RENAME,
	mxID_PROJECT_POPUP_COMPILING_OPTS,
	mxID_PROJECT_POPUP_COMPILE_NOW,
	mxID_PROJECT_POPUP_COMPILE_FIRST,
	mxID_PROJECT_POPUP_DELETE,
	mxID_PROJECT_POPUP_MOVE_TO_SOURCES,
	mxID_PROJECT_POPUP_MOVE_TO_HEADERS,
	mxID_PROJECT_POPUP_MOVE_TO_OTHERS,
	mxID_PROJECT_POPUP_ADD,
	mxID_PROJECT_POPUP_ADD_MULTI,
	mxID_PROJECT_POPUP_ADD_SELECTED,

	mxID_PROJECT_CONFIG_BYSRC,
	mxID_PROJECT_CONFIG_CUSTOM_TABS,
	mxID_PROJECT_CONFIG_AUTOIMPROVE_TEMPLATES,
	mxID_PROJECT_CONFIG_NAME,
	mxID_PROJECT_CONFIG_MANIFEST_DIR,
	mxID_PROJECT_CONFIG_ICON_DIR,
	mxID_PROJECT_CONFIG_ENV_VARS,
	mxID_PROJECT_CONFIG_TEMP_DIR,
	mxID_PROJECT_CONFIG_WORKING_DIR,
	mxID_PROJECT_CONFIG_EXEC_METHOD,
	mxID_PROJECT_CONFIG_EXEC_SCRIPT,
	mxID_PROJECT_CONFIG_ADD,
	mxID_PROJECT_CONFIG_SELECT,
	mxID_PROJECT_CONFIG_REMOVE,
	mxID_PROJECT_CONFIG_RENAME,
	mxID_PROJECT_CONFIG_APPLY,
	mxID_PROJECT_CONFIG_STEPS_RUN,
	mxID_PROJECT_CONFIG_STEPS_ADD,
	mxID_PROJECT_CONFIG_STEPS_DEL,
	mxID_PROJECT_CONFIG_STEPS_UP,
	mxID_PROJECT_CONFIG_STEPS_DOWN,
	mxID_PROJECT_CONFIG_STEPS_EDIT,
	mxID_PROJECT_CONFIG_TOOLCHAIN_COMBO,
	mxID_PROJECT_CONFIG_TOOLCHAIN_OPTIONS,
	mxID_PROJECT_CONFIG_IMPORT_LIBS,
		
	mxID_PROJECT_CONFIG_LIBS_EDIT,
	mxID_PROJECT_CONFIG_LIBS_ADD,
	mxID_PROJECT_CONFIG_LIBS_DEL,
	mxID_PROJECT_CONFIG_LIBS_DONT_EXE,
		
	mxID_PROJECT_CONFIG_ARGS_BUTTON,
	mxID_PROJECT_CONFIG_COMPILE_EXTRA_BUTTON,
	mxID_PROJECT_CONFIG_COMPILE_MACROS_BUTTON,
	mxID_PROJECT_CONFIG_COMPILE_DIRS_BUTTON,
	mxID_PROJECT_CONFIG_LINK_EXTRA_BUTTON,
	mxID_PROJECT_CONFIG_LINK_LIBS_BUTTON,
	mxID_PROJECT_CONFIG_LINK_DIRS_BUTTON,
	mxID_PROJECT_GENERAL_EXE_PATH,
		
		
	mxID_HELP_BUTTON,

	mxPROCESS_COMPILE,
	mxPROCESS_DEBUG,
	mxPROCESS_DEBUG_TERM,
	mxPROCESS_RUN,

	mxID_TIP_OTHERONE,
	mxID_TIP_CLOSE,

	mxID_SOCKET_SERVER,
	mxID_SOCKET_UPDATES,

	mxID_SHARE_GET_LIST,
	mxID_SHARE_CLIENTS_LIST,
	mxID_SHARE_FILES_LIST,

	mxID_WIZARD_NEW_FILE_PATH,
	mxID_WIZARD_START_RADIO,
	mxID_WIZARD_ONPROJECT_NAME,
	mxID_WIZARD_ONPROJECT_RADIO,
	mxID_WIZARD_PROJECT_PATH_RADIO,
	mxID_WIZARD_PROJECT_NAME_TEXT,
	mxID_WIZARD_PROJECT_FOLDER_TEXT,
	mxID_WIZARD_PROJECT_FOLDER_CHECK,
	mxID_WIZARD_PROJECT_FILES_OPEN_CHECK,
	mxID_WIZARD_PROJECT_FILES_DIR_CHECK,

	mxID_MAX_JOBS,
	mxID_AUTOCODES_FILE,
	mxID_AUTOCODES_OPEN,
	mxID_AUTOCODES_EDIT,
	mxID_WIZARD_FOLDER,
	mxID_WORKING_FOLDER,
	mxID_TEMPLATES_FOLDER,
	mxID_MINGW_FOLDER,
	mxID_QUICKHELP_FOLDER,
	mxID_TEMP_FOLDER,
	mxID_PROJECTS_FOLDER,
	mxID_AUTOCOMP_FOLDER,
	mxID_WXHELP_FOLDER,
	mxID_SKIN_APPLY,
	mxID_SKIN_LIST,
	mxID_GCC_PATH,
	mxID_GPP_PATH,
	mxID_GDB_PATH,
	mxID_IMG_VIEWER_PATH,
	mxID_XDOT_PATH,
	mxID_BROWSER_PATH,
	mxID_WXFB_PATH,
	mxID_VALGRIND_PATH,
	mxID_CPPCHECK_PATH,
	mxID_DOXYGEN_PATH,
	mxID_DEBUG_MACROS_EDIT,
	mxID_DEBUG_MACROS_OPEN,
	mxID_DEBUG_MACROS,
	mxID_DEBUG_BLACKLIST,
	mxID_PREFERENCES_CLEAR_SUBCMD_CACHE,
	mxID_PREFERENCES_CUSTOMIZE_SHORTCUTS,
	mxID_PREFERENCES_FONTSIZE,
	mxID_PREFERENCES_FONTNAME,
	mxID_PREFERENCES_XDG,
	mxID_PREFERENCES_TOOLCHAIN_OPTIONS,
	mxID_PREFERENCES_TOOLBAR_FILE,
	mxID_PREFERENCES_TOOLBAR_EDIT,
	mxID_PREFERENCES_TOOLBAR_VIEW,
	mxID_PREFERENCES_TOOLBAR_DEBUG,
	mxID_PREFERENCES_TOOLBAR_MISC,
	mxID_PREFERENCES_TOOLBAR_TOOLS,
	mxID_PREFERENCES_TOOLBAR_RUN,
	mxID_PREFERENCES_TOOLBAR_RESET,
	mxID_TOOLBAR_SETTINGS,
		
	mxID_TIMER_AFTER_EVENTS,
	mxID_TIMER_OSD,
	mxID_TIMER_OUTPUT,
	mxID_TIMER_PROC,
	mxID_TIMER_INPUT,
		
	mxID_HELPW,
	mxID_HELPW_HIDETREE,
	mxID_HELPW_HOME,
	mxID_HELPW_NEXT,
	mxID_HELPW_PREV,
	mxID_HELPW_COPY,
	mxID_HELPW_FORUM,
	mxID_HELPW_PRINT,
	mxID_HELPW_SEARCH,
	mxID_HELPW_SEARCH_ALL,
		
	mxID_HIDE_PANEL_TIMER,
	mxID_COMPILER_TIMER,
	mxID_PARSER_TIMER,
	mxID_PARSER_PROCESS_TIMER,
		
	mxID_MAKEFILE_EXPLORE,
		
	mxID_AUTOCOMP_LIST,
		
	mxID_FIND_SCOPE,
	mxID_FIND_FIND_NEXT,
	mxID_FIND_FIND_PREV,
	mxID_FIND_REPLACE,
	mxID_FIND_REPLACE_ALL,
	mxID_FIND_CANCEL,

	mxID_TOOLBAR_FIND,
		
	mxID_INSPECTION_EDIT,
	mxID_INSPECTION_DEREF_PTR,
	mxID_INSPECTION_COPY_ALL,
	mxID_INSPECTION_FREEZE,
	mxID_INSPECTION_UNFREEZE,
	mxID_INSPECTION_DUPLICATE,
	mxID_INSPECTION_BREAK,
	mxID_INSPECTION_RESCOPE,
	mxID_INSPECTION_FROM_CLIPBOARD,
	mxID_INSPECTION_FROM_SOURCE,
	mxID_INSPECTION_EXPLORE,
	mxID_INSPECTION_EXPLORE_ALL,
	mxID_INSPECTION_SHOW_IN_HISTORY,
	mxID_INSPECTION_SHOW_IN_TABLE,
	mxID_INSPECTION_SHOW_IN_TEXT,
	mxID_INSPECTION_SHOW_IN_RTEDITOR,
	mxID_INSPECTION_SET_WATCH_READ,
	mxID_INSPECTION_SET_WATCH_WRITE,
	mxID_INSPECTION_SET_WATCH_BOTH,
	mxID_INSPECTION_COPY_EXPRESSION,
	mxID_INSPECTION_COPY_VALUE,
	mxID_INSPECTION_COPY_TYPE,
	mxID_INSPECTION_CLEAR_ALL,
	mxID_INSPECTION_CLEAR_ONE,
	mxID_INSPECTION_SAVE_TABLE,
	mxID_INSPECTION_LOAD_TABLE,
	mxID_INSPECTION_MANAGE_TABLES,
	mxID_INSPECTION_SET_FRAMELESS,
	mxID_INSPECTION_SHOW_APPART,
		
	mxID_INSPECTION_WATCH_READ,
	mxID_INSPECTION_WATCH_WRITE,
	mxID_INSPECTION_WATCH_RW,
		
	mxID_INSPECTION_FORMAT_NAT,
	mxID_INSPECTION_FORMAT_DEC,
	mxID_INSPECTION_FORMAT_HEX,
	mxID_INSPECTION_FORMAT_BIN,
	mxID_INSPECTION_FORMAT_OCT,
		
	mxID_INSPECTION_IMPR_ADD_GENERAL,
	mxID_INSPECTION_IMPR_ADD_PROJECT,
	mxID_INSPECTION_IMPR_EXPOSE,
	mxID_INSPECTION_IMPR_DISCARD,
	mxID_INSPECTION_IMPR_CONF_GENERAL,
	mxID_INSPECTION_IMPR_CONF_PROJECT,
		
	mxID_INSPECTION_TREE,
		
	mxID_INSPHISTORY_LOG_ALL,
	mxID_INSPHISTORY_LOG_NONE,
	mxID_INSPHISTORY_LOG_CHANGE,
	mxID_INSPHISTORY_CLEAR_LOG,
		
	mxID_BACKTRACE_EXPLORE_ARGS,
	mxID_BACKTRACE_EXPLORE_LOCALS,
	mxID_BACKTRACE_GOTO_POS,
	mxID_BACKTRACE_INSPECT_ARGS,
	mxID_BACKTRACE_INSPECT_LOCALS,
	mxID_BACKTRACE_UPDATE,
	mxID_BACKTRACE_ADD_FILE_TO_BLACKLIST,
//	mxID_BACKTRACE_ADD_FUNCTION_TO_BLACKLIST,
		
	mxID_EXEINFO_STRIP,
		
	mxID_BREAK_LIST_GOTO,
	mxID_BREAK_LIST_EDIT,
	mxID_BREAK_LIST_DELETE_ALL,
	mxID_BREAK_LIST_DELETE,
	mxID_BREAK_OPTS_ENABLE,
	
	mxID_TERMINALS_BUTTON,

	mxID_EXPLORERS_BUTTON,
	mxID_EXPLORERS_KONQUEROR,
	mxID_EXPLORERS_NAUTILUS,
	mxID_EXPLORERS_DOLPHIN,
	mxID_EXPLORERS_THUNAR,

	mxID_ARGS_REPLACE_FILE,
	mxID_ARGS_REPLACE_DIR,
	mxID_ARGS_ADD_FILE,
	mxID_ARGS_ADD_DIR,
	mxID_ARGS_EDIT_LIST,
	mxID_ARGS_EDIT_TEXT,
	mxID_ARGS_BUTTON,
	mxID_ARGS_FROM_TEMPLATE,
	mxID_ARGS_DEFAULT,
	
	mxID_COMPILE_OPTIONS_COMP_EXTRA,
	
	mxID_ENUMED_UP,
	mxID_ENUMED_DOWN,
	mxID_ENUMED_EDIT,
	mxID_ENUMED_ADD,
	mxID_ENUMED_DELETE,
	
	mxSOURCE_AUTOCOMPLETE,
		
	mxID_PROJECT_CONFIG_CUSTOM_TOOLS,
	mxID_PROJECT_CONFIG_AUTOCOMP_INDEXES,
		
	mxID_DOXYDIALOG_DEST,
	mxID_DOXYDIALOG_BASE,
		
	mxID_VALGRIND_OPEN_OUTPUT_FILE,
	mxID_VALGRIND_RELOAD_TREE,
	mxID_VALGRIND_DELETE_FROM_TREE,
		
	mxID_COLORS_PICKER,
	mxID_DEBUG_IMPROVE_INSPECTIONS_BY_TYPE,
		
	mxID_BEGINNER_FOR,
	mxID_BEGINNER_WHILE,
	mxID_BEGINNER_DO,
	mxID_BEGINNER_IF,
	mxID_BEGINNER_SWITCH,
		
	mxID_LIBS_IN,
	mxID_LIBS_OUT,
		
	mxID_CUSTOM_PROJECT_TOOL_0,
	mxID_CUSTOM_PROJECT_TOOL_1,
	mxID_CUSTOM_PROJECT_TOOL_2,
	mxID_CUSTOM_PROJECT_TOOL_3,
	mxID_CUSTOM_PROJECT_TOOL_4,
	mxID_CUSTOM_TOOL_0,
	mxID_CUSTOM_TOOL_1,
	mxID_CUSTOM_TOOL_2,
	mxID_CUSTOM_TOOL_3,
	mxID_CUSTOM_TOOL_4,
	mxID_CUSTOM_TOOL_5,
	mxID_CUSTOM_TOOL_6,
	mxID_CUSTOM_TOOL_7,
	mxID_CUSTOM_TOOL_8,
	mxID_CUSTOM_TOOL_9,
	mxID_CUSTOM_TOOLS_COMBO,
	mxID_CUSTOM_TOOLS_COMMAND,
	mxID_CUSTOM_TOOLS_RUN,
	mxID_CUSTOM_TOOLS_WORKDIR,
		
	mxID_TOOLCHAINS_TYPE_COMBO,
		

	mxID_EXTRA_STEP_COMMAND,
	mxID_EXTRA_STEP_OUTPUT,
	mxID_EXTRA_STEP_DEPS,
		
	mxID_POPUPS_INSERT_FIRST,
	mxID_POPUPS_INSERT_LIST,
	mxID_POPUPS_INSERT_TEXT,
	mxID_POPUPS_INSERT_FILE,
	mxID_POPUPS_INSERT_DIR,
	mxID_POPUPS_INSERT_TEMP_DIR,
	mxID_POPUPS_INSERT_MINGW_DIR,
	mxID_POPUPS_INSERT_BROWSER,
	mxID_POPUPS_INSERT_SHELL_EXECUTE,
	mxID_POPUPS_INSERT_PROJECT_PATH,
	mxID_POPUPS_INSERT_PROJECT_BIN,
	mxID_POPUPS_INSERT_ARGS,
//	mxID_POPUPS_INSERT_WORK_DIR,
	mxID_POPUPS_INSERT_CURRENT_FILE,
	mxID_POPUPS_INSERT_CURRENT_DIR,
	mxID_POPUPS_INSERT_ZINJAI_DIR,
	mxID_POPUPS_INSERT_WORKDIR,
	mxID_POPUPS_INSERT_OUTPUT,
	mxID_POPUPS_INSERT_DEPS,
	mxID_POPUPS_INSERT_LAST,
		
	mxID_MULTIPLEFILE_DIR,
	mxID_MULTIPLEFILE_FIND,
	mxID_MULTIPLEFILE_MARK_NONE,
	mxID_MULTIPLEFILE_MARK_ALL,
	mxID_MULTIPLEFILE_MARK_INVERT,
	mxID_MULTIPLEFILE_ADD,
	mxID_MULTIPLEFILE_CANCEL,
	
	mxID_CPPCHECK_COPYCONFIG,
	mxID_CPPCHECK_CONFIG_D,
	mxID_CPPCHECK_CONFIG_U,
	mxID_CPPCHECK_STYLE,
	mxID_CPPCHECK_PLATFORM,
	mxID_CPPCHECK_STANDARD,
	mxID_CPPCHECK_SUPPRESS_FILE,
	mxID_CPPCHECK_SUPPRESS_IDS,
	mxID_CPPCHECK_INCLUDE_FILE,
	mxID_CPPCHECK_EXCLUDE_FILE,
	mxID_CPPCHECK_EXCLUDE_HEADERS,
	mxID_CPPCHECK_ADDITIONAL_FILES,
		
	mxID_INSPECTION_TEMPLATES_LIST,
	mxID_INSPECTION_TEMPLATES_FROM,
	mxID_INSPECTION_TEMPLATES_TO,
	mxID_INSPECTION_TEMPLATES_ADD,
	mxID_INSPECTION_TEMPLATES_DEL,
	mxID_INSPECTION_TEMPLATES_UP,
	mxID_INSPECTION_TEMPLATES_DOWN,
		
	mxID_EXTERN_COMPILER_OUTPUT,
		
	mxID_COL_ID,
	mxID_COL_ID_0,
	mxID_COL_ID_1,
	mxID_COL_ID_2,
	mxID_COL_ID_3,
	mxID_COL_ID_4,
	mxID_COL_ID_5,
	mxID_COL_ID_6,
	mxID_COL_ID_7,
	mxID_COL_ID_8,
	mxID_COL_ID_9,
		
	mxID_LAST_ID
};

#endif