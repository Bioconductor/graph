#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/RConverters.h>
#include <R_ext/Rdynload.h>

SEXP R_scalarString(const char *);
SEXP intersectStrings(SEXP, SEXP);
SEXP graphIntersection(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP checkEdgeList(SEXP, SEXP);

static const R_CallMethodDef R_CallDef[] = {
    {"intersectStrings", (DL_FUNC)&intersectStrings, 2},
    {"graphIntersection", (DL_FUNC)&graphIntersection, 5},
    {NULL, NULL, 0},
};

void R_init_graph(DllInfo *info) {
    R_registerRoutines(info, NULL, R_CallDef, NULL, NULL);
}


SEXP 
R_scalarString(const char *v)
{
  SEXP ans = allocVector(STRSXP, 1);
  PROTECT(ans);
  if(v)
    SET_STRING_ELT(ans, 0, mkChar(v));
  UNPROTECT(1);
  return(ans);
}

SEXP intersectStrings(SEXP x, SEXP y) {
    SEXP ans, matchRes, matched, dup;
    int i, j, k, n, numZero=0, size;
    int curEntry=0;

    matchRes = Rf_match(y, x, 0);
    
    for (i = 0; i < length(matchRes); i++) {
	if (INTEGER(matchRes)[i] == 0)
	    numZero++;
    }

    size = length(matchRes) - numZero;
    PROTECT(matched = allocVector(STRSXP, size));

    for (i = 0; i < length(matchRes); i++) {
	if (INTEGER(matchRes)[i] != 0) {
	    SET_STRING_ELT(matched, curEntry++, 
			   STRING_ELT(y, INTEGER(matchRes)[i]-1));
	}
    }

    PROTECT(dup = Rf_duplicated(matched));
    n = length(matched);

    k = 0;
    for (j = 0; j < n; j++)
	if (LOGICAL(dup)[j] == 0)
	    k++;

    PROTECT(ans = allocVector(STRSXP, k));
    k = 0;
    for (j = 0; j < n; j++) {
	if (LOGICAL(dup)[j] == 0) {
	    SET_STRING_ELT(ans, k++, STRING_ELT(matched, j));
	}
    }

    UNPROTECT(3);
    return(ans);
}


SEXP graphIntersection(SEXP xN, SEXP yN, SEXP xE, SEXP yE, 
		       SEXP edgeMode) {
    /* edgeMode == 0 implies "undirected" */
    SEXP bN, newXE, newYE;
    SEXP klass, outGraph;
    SEXP rval, ans, curRval, curWeights, curEdges, newNames, matches;
    int i, j, curEle=0;

    klass = MAKE_CLASS("graphNEL");
    PROTECT(outGraph = NEW_OBJECT(klass));
    if (INTEGER(edgeMode)[0])
	SET_SLOT(outGraph, Rf_install("edgemode"),  
		 R_scalarString("directed"));
    else
	SET_SLOT(outGraph, Rf_install("edgemode"),
		 R_scalarString("undirected"));    

    bN = intersectStrings(xN, yN);

    if (length(bN) == 0) {
	SET_SLOT(outGraph, Rf_install("nodes"), allocVector(STRSXP, 0));
	SET_SLOT(outGraph, Rf_install("edgeL"), allocVector(VECSXP, 0));
	UNPROTECT(1);
	return(outGraph);
    }

    newXE = checkEdgeList(xE, bN);
    newYE = checkEdgeList(yE, bN);

    PROTECT(newNames = allocVector(STRSXP, 2));
    SET_STRING_ELT(newNames, 0, mkChar("edges"));
    SET_STRING_ELT(newNames, 1, mkChar("weights"));

    PROTECT(rval = allocVector(VECSXP, length(newXE)));
    for (i = 0; i < length(newXE); i++) {
	PROTECT(curRval = allocVector(VECSXP, 2));
	setAttrib(curRval, R_NamesSymbol, newNames);

	ans = intersectStrings(VECTOR_ELT(newXE, i), VECTOR_ELT(newYE,
								i));
	if (length(ans) == 0) {
	    SET_VECTOR_ELT(curRval, 0, allocVector(INTSXP, 0));
	    SET_VECTOR_ELT(curRval, 1, allocVector(INTSXP, 0));
	}
	else {
	    PROTECT(curEdges = allocVector(INTSXP, length(ans)));
	    matches = Rf_match(bN, ans, 0);
	    curEle = 0;
	    for (j = 0; j < length(matches); j++) {
		if (INTEGER(matches)[j] != 0)
		    INTEGER(curEdges)[curEle++] = INTEGER(matches)[j];
	    }
	    SET_VECTOR_ELT(curRval, 0, curEdges);
	    PROTECT(curWeights = allocVector(INTSXP, length(ans)));
	    for (j = 0; j < length(ans); j++)
		INTEGER(curWeights)[j] = 1;
	    SET_VECTOR_ELT(curRval, 1, curWeights);
	    UNPROTECT(2);
	}
	SET_VECTOR_ELT(rval, i, curRval);
	UNPROTECT(1);
    }
    setAttrib(rval, R_NamesSymbol, bN);
    
    SET_SLOT(outGraph, Rf_install("nodes"), bN);
    SET_SLOT(outGraph, Rf_install("edgeL"), rval);

    UNPROTECT(3);
    return(outGraph);
}

SEXP checkEdgeList(SEXP eL, SEXP bN) {
    SEXP newEL, curVec, curMatches, newVec, eleNames;
    int i, j, k, size, curEle;

    PROTECT(newEL = allocVector(VECSXP, length(bN)));
    PROTECT(eleNames = getAttrib(eL, R_NamesSymbol));
    for (i = 0; i < length(bN); i++) {
	for (k = 0; k < length(eL); k++) {
	    if (strcmp(CHAR(STRING_ELT(eleNames, k)),
		       CHAR(STRING_ELT(bN, i))) == 0)
		break;
	}
	if (k < length(eL)) {
	    curVec = VECTOR_ELT(eL, k);
	    curMatches = Rf_match(curVec, bN, 0);
	    size = length(curMatches);
	    for (j = 0; j < length(curMatches); j++) {
		if (INTEGER(curMatches)[j] == 0)
		    size--;
	    }
	    PROTECT(newVec = allocVector(STRSXP, size));
	    curEle = 0;
	    for (j = 0; j < length(curMatches); j++) {
		if (INTEGER(curMatches)[j] != 0) {
		    SET_STRING_ELT(newVec, curEle++, 
				   STRING_ELT(curVec,
					      INTEGER(curMatches)[j]-1));
		}
	    }
	    SET_VECTOR_ELT(newEL, i, newVec);
	}
    }
    setAttrib(newEL, R_NamesSymbol, bN);
    UNPROTECT(length(newEL)+2);
    return(newEL);
}
