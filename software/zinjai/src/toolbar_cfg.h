#ifndef TOOLBAR_CFG_H
#define TOOLBAR_CFG_H
#include <wx/string.h>

struct toolbar_item_struct {
	wxString config_key;
	bool visible;
	wxWindowID command_id;
	int name_langid;
	const char *name_default;
	int status_langid;
	const char *status_default;
	const char *icon_fname;
};


struct toolbar_struct {
	wxString config_key;
	bool visible;
	int name_langid;
	const char *name_default;
	toolbar_item_struct *items;
	int size,count;
	toolbar_struct() {}
	toolbar_struct(wxString _config_key, bool _visible, int _name_langid, const char *_name_default, int _size) : 
		config_key(_config_key), visible(_visible), name_langid(_name_langid), name_default(_name_default), items(NULL), size(_size), count(0)  {
			items = new toolbar_item_struct[size];
		}
	void Add(wxString config_key, wxWindowID command_id, int name_langid, const char *name_default, const char *icon_fname, int status_langid, const char *status_default) {
		if (count+1<=size) asm("int3");
		items[count].config_key=config_key;
		items[count].command_id=command_id;
		items[count].name_langid=name_langid;
		items[count].name_default=name_default;
		items[count].status_langid=status_langid;
		items[count].status_default=status_default;
		items[count].icon_fname=icon_fname;
		count++;
	}
};

enum TOOLBARS { TB_FILE=0, TB_EDIT, TB_RUN, TB_MISC, TB_VIEW, TB_TOOLS, TB_DEBUG, TB_COUNT };
	
void InitToolbarInfo();
const toolbar_struct *GetToolbarInfo(int toolbar_index);
const toolbar_item_struct *GetToolbarInfo(int toolbar_index, int button_index);

#endif

