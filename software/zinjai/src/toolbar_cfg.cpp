#include "toolbar_cfg.h"
#include "Language.h"
#include "ids.h"


toolbar_struct data[TB_COUNT];

void InitToolbarInfo() {
	data[TB_FILE]=toolbar_struct("file",true,LANG_PREFERENCES_TOOLBARS_FILE,"Archivo",18);
	data[TB_FILE].Add("new_file",wxID_NEW,LANG_TOOLBAR_CAPTION_FILE_NEW,"Nuevo...","nuevo.png",LANG_TOOLBAR_DESC_FILE_NEW,"Archivo -> Nuevo...");
	data[TB_FILE].Add("new_project",mxID_FILE_PROJECT,LANG_TOOLBAR_CAPTION_FILE_NEW_PROJECT,"Nuevo Proyecto...","proyecto.png",LANG_TOOLBAR_DESC_FILE_NEW_PROJECT,"Archivo -> Nuevo Proyecto...");
	data[TB_FILE].Add("open",wxID_OPEN,LANG_TOOLBAR_CAPTION_FILE_OPEN,"Abrir...","abrir.png",LANG_TOOLBAR_DESC_FILE_OPEN,"Archivo -> Abrir...");
	data[TB_FILE].Add("recent_simple",mxID_FILE_SOURCE_HISTORY_MORE,LANG_TOOLBAR_CAPTION_FILE_RECENT_SOURCES,"Fuentes Recientes...","recentSimple.png",LANG_TOOLBAR_DESC_FILE_RECENT_SOURCES,"Archivo -> Fuentes Abiertos Recientemente...");
	data[TB_FILE].Add("recent_project",mxID_FILE_PROJECT_HISTORY_MORE,LANG_TOOLBAR_CAPTION_FILE_RECENT_PROJECTS,"Proyectos Recientes...","recentProject.png",LANG_TOOLBAR_DESC_FILE_RECENT_PROJECTS,"Archivo -> Proyectos Abiertos Recientemete...");
	data[TB_FILE].Add("open_header",mxID_FILE_OPEN_H,LANG_TOOLBAR_CAPTION_FILE_OPEN_H,"Abrir h/cpp Complementario","abrirp.png",LANG_TOOLBAR_DESC_FILE_OPEN_H,"Abrir h/cpp Complementario");
	data[TB_FILE].Add("open_selected",mxID_FILE_OPEN_SELECTED,LANG_TOOLBAR_CAPTION_FILE_OPEN_SELECTED,"Abrir Seleccionado","abrirh.png",LANG_TOOLBAR_DESC_FILE_OPEN_SELECTED,"Archivo -> Abrir Seleccionado");
	data[TB_FILE].Add("save",wxID_SAVE,LANG_TOOLBAR_CAPTION_FILE_SAVE,"Guardar","guardar.png",LANG_TOOLBAR_DESC_FILE_SABE,"Archivo -> Guardar");
	data[TB_FILE].Add("save_as",wxID_SAVEAS,LANG_TOOLBAR_CAPTION_FILE_SAVE_AS,"Guardar Como...","guardarComo.png",LANG_TOOLBAR_DESC_FILE_SAVE_AS,"Archivo -> Guardar Como...");
	data[TB_FILE].Add("save_all",mxID_FILE_SAVE_ALL,LANG_TOOLBAR_CAPTION_FILE_SAVE_ALL,"Guardar Todo","guardarTodo.png",LANG_TOOLBAR_DESC_FILE_SAVE_ALL,"Archivo -> Guardar Todo");
	data[TB_FILE].Add("save_project",mxID_FILE_SAVE_PROJECT,LANG_TOOLBAR_CAPTION_FILE_SAVE_PROJECT,"Guardar Proyecto","guardarProyecto.png",LANG_TOOLBAR_DESC_FILE_SAVE_PROJECT,"Archivo -> Guardar Proyecto");
	data[TB_FILE].Add("export_html",mxID_FILE_EXPORT_HTML,LANG_TOOLBAR_CAPTION_FILE_EXPORT_HTML,"Exportar a HTML...","exportHtml.png",LANG_TOOLBAR_DESC_FILE_EXPORT_HTML,"Archivo -> Exportar a HTML...");
	data[TB_FILE].Add("print",mxID_FILE_PRINT,LANG_TOOLBAR_CAPTION_FILE_PRINT,"Imprimir...","imprimir.png",LANG_TOOLBAR_DESC_FILE_PRINT,"Archivo -> Imprimir...");
	data[TB_FILE].Add("reload",mxID_FILE_RELOAD,LANG_TOOLBAR_CAPTION_FILE_RELOAD,"Recargar","recargar.png",LANG_TOOLBAR_DESC_FILE_RELOAD,"Archivo -> Recargar");
	data[TB_FILE].Add("close",wxID_CLOSE,LANG_TOOLBAR_CAPTION_FILE_CLOSE,"Cerrar","cerrar.png",LANG_TOOLBAR_DESC_FILE_CLOSE,"Archivo -> Cerrar");
	data[TB_FILE].Add("close_all",mxID_FILE_CLOSE_ALL,LANG_TOOLBAR_CAPTION_FILE_CLOSE_ALL,"Cerrar Todo","cerrarTodo.png",LANG_TOOLBAR_DESC_FILE_CLOSE_ALL,"Archivo -> Cerrar Todo");
	data[TB_FILE].Add("close_project",mxID_FILE_CLOSE_PROJECT,LANG_TOOLBAR_CAPTION_FILE_CLOSE_PROJECT,"Cerrar Proyecto","cerrarProyecto.png",LANG_TOOLBAR_DESC_FILE_CLOSE_PROJECT,"Archivo -> Cerrar Proyecto");
	data[TB_FILE].Add("project_config",mxID_FILE_PROJECT_CONFIG,LANG_TOOLBAR_CAPTION_FILE_PROJECT_CONFIG,"Configuracion de proyecto...","projectConfig.png",LANG_TOOLBAR_DESC_FILE_PROJECT_CONFIG,"Archivo -> Configuracion del Proyecto...");
}

const toolbar_struct *GetToolbarInfo(int toolbar_index) {
	if (toolbar_index>=TB_COUNT) return NULL;
	else return &(data[toolbar_index]);
}

const toolbar_item_struct *GetToolbarItemInfo(int toolbar_index, int button_index) { 
	if (button_index>=data[toolbar_index].count) return NULL;
	else return &(data[toolbar_index].items[button_index]);
}

