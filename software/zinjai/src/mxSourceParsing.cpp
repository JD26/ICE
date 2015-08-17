//#include "mxSource.h"
//
//#define GetCharAt src->GetCharAt
//#define GetStyleAt src->GetStyleAt
//
//#define II_BACK(p,a) while(p>0 && (a)) p--;
//#define II_BACK_NC(p,a) while(a) p--;
//#define II_FRONT(p,a) while(p<l && (a)) p++;
//#define II_FRONT_NC(p,a) while(a) p++;
//#define II_IS_2(p,c1,c2) ((c=GetCharAt(p))==c1 || c==c2)
//#define II_IS_3(p,c1,c2,c3) ((c=GetCharAt(p))==c1 || c==c2 || c==c3)
//#define II_IS_4(p,c1,c2,c3,c4) ((c=GetCharAt(p))==c1 || c==c2 || c==c3 || c==c4)
//#define II_IS_5(p,c1,c2,c3,c4,c5) ((c=GetCharAt(p))==c1 || c==c2 || c==c3 || c==c4 || c==c5)
//#define II_IS_6(p,c1,c2,c3,c4,c5,c6) ((c=GetCharAt(p))==c1 || c==c2 || c==c3 || c==c4 || c==c5 || c==c6)
//#define II_IS_COMMENT(p) ((s=GetStyleAt(p))==wxSTC_C_COMMENT || s==wxSTC_C_COMMENTLINE || s==wxSTC_C_COMMENTLINEDOC || s==wxSTC_C_COMMENTDOC || s==wxSTC_C_COMMENTDOCKEYWORD || s==wxSTC_C_COMMENTDOCKEYWORDERROR)
//#define II_SHOULD_IGNORE(p) ((s=GetStyleAt(p))==wxSTC_C_COMMENT || s==wxSTC_C_COMMENTLINE || s==wxSTC_C_CHARACTER || s==wxSTC_C_STRING || s==wxSTC_C_COMMENTLINEDOC || s==wxSTC_C_COMMENTDOC || s==wxSTC_C_PREPROCESSOR || s==wxSTC_C_COMMENTDOCKEYWORD || s==wxSTC_C_COMMENTDOCKEYWORDERROR)
//#define II_IS_NOTHING_4(p) (II_IS_4(p,' ','\t','\r','\n') || II_SHOULD_IGNORE(p))
//#define II_IS_NOTHING_2(p) (II_IS_2(p,' ','\t') || II_SHOULD_IGNORE(p)) 
//#define II_IS_KEYWORD_CHAR(c) ( ( (c|32)>='a' && (c|32)<='z' ) || (c>='0' && c<='9') || c=='_' )
//
//int GetBeggining(mxSource *src, int p) {
//	char c=GetCharAt(p);
//	if (p==')') { //
//		int r=src->BraceMatch(p);
//		if (r==wxSTC_INVALID_POSITION) return p;
//		else return r;
//	}
//	else if (p==']') {
//		int r=src->BraceMatch(p);
//		if (r==wxSTC_INVALID_POSITION) return p;
//		p=r;
//	}
//	if 
//}
//
//void mxSource::OnEditForceAutoComplete(wxCommandEvent &evt) {
//	SetSelectionStart(GetBeggining(this,GetCurrentPos()));
//	SetSelectionEnd(GetCurrentPos());
//}
//
//
//
//
/////// return values for mxSource::GetTypeOf
////enum MXS_PARSE_WHAT {
////		MXS_PW_NONE=0, ///< unknown
////		MXS_PW_NOTHING, ///< var type, attrib type, or return type, for autocomplete
////		MXS_PW_TYPE, ///< var type, attrib type, or return type, for autocomplete
////		MXS_PW_NAME, ///< class name or function name, for calltips
////		MXS_PW_WRONG, ///< something is wrong in the expression
////		MXS_PW_NONE, ///< not a type or space (for recursive calls)
////	};
////
////
/////**
////* Dado el final de una palabra o llamada a funcion, averigua qué es
////**/
////bool mxSource::Booga(int pos, wxString &scope, wxString &type, int &dims, bool full) {
////	// la idea es volver al principio de la instruccion, para ir extrayendo los 
////	// tokens en orden y analizando a medida que salen
////	int p=pos;
////	while (p>0) {
////		if (II_IS_NOTHING_4(p)) p--;
////		else if (s==wxSTC_C_STRING||s==wxSTC_C_CHARACTER)
////		else if (c==')' || c==']' || c=='
////	}
////	p++;
////}
////
////
////bool mxSource::Booga(int pos, wxString &scope, wxString &type, int &dims, bool full) {
////	
////	int s, p=pos, delta_dims=0; char c;
////	II_BACK(p,II_IS_NOTHING_4(p)); if (p<0) return;
////	
////	if (c==']') { // arreglo o sobrecarga del operador []
////		p=BraceMatch(p); mdims--; if (p==wxSTC_INVALID_POSITION) return MXS_ERROR; // buscar el otro corchete
////		p--; Booga(p,scope,type,dims); // determinar qué era lo que estaba antes del corchete
////		...
////	} else if (c=='.' && s!=wxSTC_C_NUMBER) { // llamada a método desde objeto
////		...
////	} else if (c=='>' && p>0 && GetCharAt(p-1)=='-') { // llamada a método desde puntero
////		...
////	} else if (c=='*') { // argumento de template o cast tipo c++
////		...
////	} else if (c==':' && p>0 && GetCharAt(p-1)==':') { // scope::
////		...
////	} else if (c=='>') { // argumento de template o cast tipo c++? (ojo con los operadores >> y > !!!)
////		...
////	} else if (c==')') { // llamada a funcion/metodo, constructor, o cast tipo c, o solo parentesis por jerarquia?
////		...
////	} else if (s==wxSTC_C_NUMBER) { // constante numérica
////		scope=""; type="double"; dim=0;
////		return true;
////	} else if (c=='\'' || c=='\"') { // constante cstring
////		scope=""; type="char"; 
////		if (c=='\'') dims=0; else dims=1;
////		return true;
////	} else if (II_IS_KEYWORD_CHAR) { // identificador (variable, o solo nombre de funcion/metodo/clase)
////		int pend=p; p=WordStartPosition(p);
////		key=GetTextRange(p,pend); // key será esa palabra...
////		wxString aux; p--; 
////		if (Booga(p,aux,scope,dims)) { // ...pero hay que ver de donde viene (scope.key)
////			
////		}
////		scope=""; dims=0; return true;
////	} else { // nada, hasta ahí llegó
////		return false;
////	}
////}
////	// todo, ver que hacer en las llamadas recursivas, porque (1+string) va a dar string
////	
////	// si era un arreglo, puede haber índices antes del nombre, ej: bar[i][j]
////	
////	while (c==']') {
////		p=BraceMatch(p); mdims--;
////		if (p==wxSTC_INVALID_POSITION) return MXS_PW_WRONG;
////		p--; II_BACK(p,II_IS_NOTHING_4(p));
////	}
////	
////	// todo: ver que pasa con templates y casts al estilo c++
////	if (c==')') {  // si era una llamada a funcion/metodo/constructor, o una expresion entre parentesis
////		int p_cierra=p;
////		int p_abre=p=BraceMatch(p); // saltear argumentos/expresion
////		if (p==wxSTC_INVALID_POSITION) return MXS_WRONG;
////		p--; II_BACK(p,II_IS_NOTHING_4(p)); // buscar donde empieza el nombre si fuera funcion
////		MXS_PARSE_WHAT ret=Booga(p,scope,type,dims); 
////		if (ret==MXS_PW_NONE) { // si no había un nombre válido, puede ser solo expresion entre paréntesis... ej: (*bar)
////			// evaluar ahora que hay dentro del parentesis
////			p=p_cierra-1; II_BACK(p,II_IS_NOTHING_4(p)); 
////			ret=Booga(p,scope,type,dims,true);
////			if (full) { // si era full, considerar los asteriscos previos
////				p=p_abre-1; c=GetCharAt(p); II_BACK(p,II_IS_NOTHING_4(p));
////				while (c=='*') { dims--; p--; II_BACK(p,II_IS_NOTHING_4(p)); } 
////			}
////			dims+=delta_dims; return ret;
////		} else if (ret==MXS_PW_NAME) { // puede ser una llamada a funcion, método
////			
////		} else if (ret==MXS_PW_TYPE) { // puede ser una llamada a un constructor
////			
////		}
////	}
////	
////	if (!II_IS_KEYWORD_CHAR(c)) return MXS_NONE;
////	
////	// a esta altura estamos en una palabra (nombre de funcion, metodo, algo), cortar la palabra
////	int pend=p;
////	p=WordStartPosition(p);
////	key=GetTextRange(p,pend);
////	if (!key.Len()) return MXS_NULL;
////	
////	// ver el ambito de esa palabra (si habia un ., -> o :: antes)
////	p--; II_BACK(p,II_IS_NOTHING_4(p));
////	if (c=='.') {
////		p--; II_BACK(p,II_IS_NOTHING_4(p));
////		MXS_PARSE_WHAT ret=Booga(p,scope,type,dims);
////		if (ret==MXS_PW_TYPE) {
////			if (dims!=0) return MSX_WRONG;
////			
////		} else return MXS_PW_NULL
////	} else if (c=='>' && GetCharAt(p-1)=='-') {
////		p--; II_BACK(p,II_IS_NOTHING_4(p));
////		MXS_PARSE_WHAT ret=Booga(p,scope,type,dims);
////		if (ret==MXS_PW_TYPE) {
////			if (dims!=1) return MSX_WRONG;
////			
////		} else return MXS_PW_NULL
////	} else if (c==':' && GetCharAt(p-1)==':') {
////		p--; II_BACK(p,II_IS_NOTHING_4(p));
////		MXS_PARSE_WHAT ret=Booga(p,scope,type,dims);
////		if (ret==MXS_PW_TYPE) {
////			if (dims!=0) return MSX_WRONG;
////			
////		} else return MXS_PW_NULL
////	} else if (c==')') { // cast al estilo c?
////		p--; II_BACK(p,II_IS_NOTHING_4(p));
////		ret=Booga(p,scope,type,dims,true);
////		dims+=delta_dims; return ret;
////	}
////	
////}
