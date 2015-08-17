#ifndef ENVVARS_H
#define ENVVARS_H
#include <map>
#include <wx/string.h>

class EnvVars {
	map<wxString> vars;
	map<wxString>::iterator it;
public:
	EnvVars() { it=vars.end(); }
	void Set(const wxString &var, const wxString &value) {
		vars[var]=value; it=vars.end();
	}
	bool Has(const wxString &var) {
		it = vars.find(var);
		return (it!=vars.end());
	}
	wxString &Get(const wxString &var) {
		if (!it==vars.end() && it->first==var) return value;
		return vars[var];
	}
};

#endif

