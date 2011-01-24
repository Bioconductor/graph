#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/RConverters.h>
#include <R_ext/Rdynload.h>

SEXP R_scalarString(const char *);
SEXP intersectStrings(SEXP, SEXP);
SEXP graphIntersection(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP checkEdgeList(SEXP, SEXP);
SEXP listLen(SEXP);
SEXP graph_attrData_lookup(SEXP attrObj, SEXP keys, SEXP attr);
SEXP graph_sublist_assign(SEXP x, SEXP subs, SEXP sublist, SEXP values);
SEXP graph_is_adjacent(SEXP fromEdges, SEXP to);
SEXP graph_bitarray_sum(SEXP bits);
SEXP graph_bitarray_set(SEXP bits, SEXP idx, SEXP val);
SEXP graph_bitarray_transpose(SEXP bits);
SEXP graph_bitarray_undirect(SEXP bits);
SEXP graph_bitarray_rowColPos(SEXP bits);
SEXP graph_bitarray_subGraph(SEXP bits, SEXP _subIndx);
SEXP graph_bitarray_edgeSetToMatrix(SEXP nodes, SEXP bits,
                                    SEXP _weights, SEXP _directed);
SEXP graph_bitarray_getBitCell(SEXP bits, SEXP _from, SEXP _to); 
SEXP graph_bitarray_Union_Attrs(SEXP inputBits, SEXP cmnBits, SEXP fromOneBits,
        SEXP fromTwoBits);
SEXP graph_bitarray_Interect_Attrs(SEXP cmnBits, SEXP fromOneBits,
        SEXP fromTwoBits);
SEXP graph_bitarray_removeEdges(SEXP bits, SEXP _indx);
SEXP graph_bitarray_getEdgeAttrOrder(SEXP , SEXP , SEXP );

//SEXP graph_bitarray_getEdgeAttrPos(SEXP origBits, SEXP newBits) ;

# define graph_duplicated(x) Rf_duplicated(x, FALSE)

static const R_CallMethodDef R_CallDef[] = {
    {"intersectStrings", (DL_FUNC)&intersectStrings, 2},
    {"graphIntersection", (DL_FUNC)&graphIntersection, 5},
    {"listLen", (DL_FUNC)&listLen, 1},
    {"graph_attrData_lookup", (DL_FUNC)&graph_attrData_lookup, 3},
    {"graph_sublist_assign", (DL_FUNC)&graph_sublist_assign, 4},
    {"graph_is_adjacent", (DL_FUNC)&graph_is_adjacent, 2},
    {"graph_bitarray_rowColPos", (DL_FUNC)&graph_bitarray_rowColPos, 1},
    {"graph_bitarray_getEdgeAttrOrder", (DL_FUNC)&graph_bitarray_getEdgeAttrOrder, 3},
    {NULL, NULL, 0},
};

void R_init_BioC_graph(DllInfo *info) {
    R_registerRoutines(info, NULL, R_CallDef, NULL, NULL);
}

SEXP R_scalarString(const char *v)
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

    PROTECT(matchRes = Rf_match(y, x, 0));

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

    PROTECT(dup = graph_duplicated(matched));
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

    UNPROTECT(4);
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
    PROTECT(bN = intersectStrings(xN, yN));
    if (length(bN) == 0) {
	SET_SLOT(outGraph, Rf_install("nodes"), allocVector(STRSXP, 0));
	SET_SLOT(outGraph, Rf_install("edgeL"), allocVector(VECSXP, 0));
	UNPROTECT(1);
	return(outGraph);
    }
    PROTECT(newXE = checkEdgeList(xE, bN));
    PROTECT(newYE = checkEdgeList(yE, bN));
    PROTECT(newNames = allocVector(STRSXP, 2));
    SET_STRING_ELT(newNames, 0, mkChar("edges"));
    SET_STRING_ELT(newNames, 1, mkChar("weights"));
    PROTECT(rval = allocVector(VECSXP, length(newXE)));
    for (i = 0; i < length(newXE); i++) {
	PROTECT(curRval = allocVector(VECSXP, 2));
	setAttrib(curRval, R_NamesSymbol, newNames);

	PROTECT(ans = intersectStrings(VECTOR_ELT(newXE, i), VECTOR_ELT(newYE,
									i)));
	if (length(ans) == 0) {
	    SET_VECTOR_ELT(curRval, 0, allocVector(INTSXP, 0));
	    SET_VECTOR_ELT(curRval, 1, allocVector(INTSXP, 0));
	}
	else {
	    PROTECT(curEdges = allocVector(INTSXP, length(ans)));
	    PROTECT(matches = Rf_match(bN, ans, 0));
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
	    UNPROTECT(3);
	}
	SET_VECTOR_ELT(rval, i, curRval);
	UNPROTECT(2);
    }
    setAttrib(rval, R_NamesSymbol, bN);
    SET_SLOT(outGraph, Rf_install("nodes"), bN);
    SET_SLOT(outGraph, Rf_install("edgeL"), rval);

    UNPROTECT(6);
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
	    PROTECT(curMatches = Rf_match(curVec, bN, 0));
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
	    UNPROTECT(2);
	}
    }
    setAttrib(newEL, R_NamesSymbol, bN);
    UNPROTECT(2);
    return(newEL);
}

/* Taken from Biobase to avoid depending on it */
SEXP listLen(SEXP x)
{
  SEXP ans;
  int i;

  if( !Rf_isNewList(x) )
    error("require a list");

  PROTECT(ans = allocVector(REALSXP, length(x)));

  for(i=0; i<length(x); i++)
    REAL(ans)[i] = length(VECTOR_ELT(x, i));
  UNPROTECT(1);
  return(ans);
}

static SEXP graph_getListElement(SEXP list, const char *str, SEXP defaultVal)
{
    SEXP elmt = defaultVal;
    SEXP names = getAttrib(list, R_NamesSymbol);
    int i;

    for (i = 0; i < length(list); i++)
        if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
            elmt = VECTOR_ELT(list, i);
            break;
        }
    return elmt;
}

static int graph_getListIndex(SEXP list, SEXP name)
{
    SEXP names = GET_NAMES(list);
    int i;
    const char* str = CHAR(STRING_ELT(name, 0));

    for (i = 0; i < length(list); i++)
        if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0)
            return i;
    return -1;
}

static SEXP graph_sublist_lookup(SEXP x, SEXP subs, SEXP sublist,
                                 SEXP defaultVal)
{
    SEXP ans, idx, names, el;
    int ns, i, j;
    sublist = STRING_ELT(sublist, 0);
    ns = length(subs);
    names = GET_NAMES(x);
    PROTECT(idx = match(names, subs, -1));
    PROTECT(ans = allocVector(VECSXP, ns));
    for (i = 0; i < ns; i++) {
        j = INTEGER(idx)[i];
        if (j < 0)
            SET_VECTOR_ELT(ans, i, defaultVal);
        else {
            el = graph_getListElement(VECTOR_ELT(x, j-1), CHAR(sublist),
                                      defaultVal);
            SET_VECTOR_ELT(ans, i, el);
        }
    }
    SET_NAMES(ans, subs);
    UNPROTECT(2);
    return ans;
}

SEXP graph_attrData_lookup(SEXP attrObj, SEXP keys, SEXP attr)
{
    SEXP data, defaults, defaultVal;
    const char* attribute;

    data = GET_SLOT(attrObj, install("data"));
    defaults = GET_SLOT(attrObj, install("defaults"));
    attribute = CHAR(STRING_ELT(attr, 0));
    /* We might want this to error out instead of grabbing a default.
       The default value should exist.
    */
    defaultVal = graph_getListElement(defaults, attribute, R_NilValue);
    return graph_sublist_lookup(data, keys, attr, defaultVal);
}

#ifdef __NOT_USED__
static SEXP graph_list_lookup(SEXP x, SEXP subs, SEXP defaultVal)
{
    SEXP ans, idx, names;
    int ns, i, j;
    ns = length(subs);
    names = GET_NAMES(x);
    PROTECT(idx = match(names, subs, -1));
    PROTECT(ans = allocVector(VECSXP, ns));
    for (i = 0; i < ns; i++) {
        j = INTEGER(idx)[i];
        if (j < 0)
            SET_VECTOR_ELT(ans, i, defaultVal); /* need to duplicate? */
        else {
            SET_VECTOR_ELT(ans, i, VECTOR_ELT(x, j-1));
        }
    }
    SET_NAMES(ans, subs);
    UNPROTECT(2);
    return ans;
}
#endif

static SEXP graph_makeItem(SEXP s, int i)
{
    if (s == R_NilValue)
	return s;
    else {
	SEXP item = R_NilValue;/* -Wall */
	switch (TYPEOF(s)) {
	case STRSXP:
            item = ScalarString(STRING_ELT(s, i));
            break;
	case EXPRSXP:
	case VECSXP:
	    item = duplicate(VECTOR_ELT(s, i));
	    break;
	case LGLSXP:
	    item = ScalarLogical(LOGICAL(s)[i]);
	    break;
	case INTSXP:
	    item = ScalarInteger(INTEGER(s)[i]);
	    break;
	case REALSXP:
	    item = ScalarReal(REAL(s)[i]);
	    break;
	case CPLXSXP:
	    item = ScalarComplex(COMPLEX(s)[i]);
	    break;
	case RAWSXP:
	    item = ScalarRaw(RAW(s)[i]);
	    break;
	default:
	    error("unknown type");
	}
	return item;
    }
}

static SEXP graph_addItemToList(SEXP list, SEXP item, SEXP name)
{
    SEXP ans, ansnames, listnames;
    int len = length(list);
    int i;

    PROTECT(ans = allocVector(VECSXP, len + 1));
    PROTECT(ansnames = allocVector(STRSXP, len + 1));
    listnames = GET_NAMES(list);
    for (i = 0; i < len; i++) {
        SET_STRING_ELT(ansnames, i, STRING_ELT(listnames, i));
        SET_VECTOR_ELT(ans, i, VECTOR_ELT(list, i));
    }
    SET_STRING_ELT(ansnames, len, STRING_ELT(name, 0));
    SET_VECTOR_ELT(ans, len, item);
    SET_NAMES(ans, ansnames);
    UNPROTECT(2);
    return ans;
}

SEXP graph_sublist_assign(SEXP x, SEXP subs, SEXP sublist, SEXP values)
{
    SEXP idx, names, tmpItem, newsubs, ans, ansnames, val;
    int ns, i, j, nnew, nextempty, origlen, numVals, tmpIdx;

    ns = length(subs);
    origlen = length(x);
    numVals = length(values);
    if (numVals > 1 && ns != numVals)
        error("invalid args: subs and values must be the same length");
    names = GET_NAMES(x);
    PROTECT(idx = match(names, subs, -1));
    PROTECT(newsubs = allocVector(STRSXP, ns));
    nnew = 0;
    for (i = 0; i < ns; i++) {
        if (INTEGER(idx)[i] == -1)
            SET_STRING_ELT(newsubs, nnew++, STRING_ELT(subs, i));
    }
    PROTECT(ans = allocVector(VECSXP, origlen + nnew));
    PROTECT(ansnames = allocVector(STRSXP, length(ans)));
    for (i = 0; i < origlen; i++) {
        SET_VECTOR_ELT(ans, i, duplicate(VECTOR_ELT(x, i)));
        SET_STRING_ELT(ansnames, i, duplicate(STRING_ELT(names, i)));
    }
    j = origlen;
    for (i = 0; i < nnew; i++)
        SET_STRING_ELT(ansnames, j++, STRING_ELT(newsubs, i));
    SET_NAMES(ans, ansnames);
    UNPROTECT(1);

    nextempty = origlen; /* index of next unfilled element of ans */
    for (i = 0; i < ns; i++) {
        if (numVals > 1)
            PROTECT(val = graph_makeItem(values, i));
        else if (numVals == 1 && isVectorList(values))
            PROTECT(val = duplicate(VECTOR_ELT(values, 0)));
        else
            PROTECT(val = duplicate(values));
        j = INTEGER(idx)[i];
        if (j < 0) {
            tmpItem = graph_addItemToList(R_NilValue, val, sublist);
            SET_VECTOR_ELT(ans, nextempty, tmpItem);
            nextempty++;
        } else {
            tmpItem = VECTOR_ELT(ans, j-1);
            tmpIdx = graph_getListIndex(tmpItem, sublist);
            if (tmpIdx == -1) {
                tmpItem = graph_addItemToList(tmpItem, val, sublist);
                SET_VECTOR_ELT(ans, j-1, tmpItem);
            } else
                SET_VECTOR_ELT(tmpItem, tmpIdx, val);
        }
        UNPROTECT(1);
    }
    UNPROTECT(3);
    return ans;
}

SEXP graph_is_adjacent(SEXP fromEdges, SEXP to)
{
    SEXP ans, frEdges, toEdge, idx;
    int i, j, lenTo;
    int found = 0;

    lenTo = length(to);
    PROTECT(ans = allocVector(LGLSXP, lenTo));
    for (i = 0; i < lenTo; i++) {
	found = 0;
        PROTECT(toEdge = ScalarString(STRING_ELT(to, i)));
        frEdges = VECTOR_ELT(fromEdges, i);
        idx = match(toEdge, frEdges, 0);
        for (j = 0; j < length(idx); j++)
            if ((found = (INTEGER(idx)[j] > 0)))
                break;
        LOGICAL(ans)[i] = found;
        UNPROTECT(1);
    }
    UNPROTECT(1);
    return ans;
}

SEXP graph_bitarray_sum(SEXP bits)
{
    /*
      This approach from
      http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan
    */
    unsigned char *bytes = (unsigned char *) RAW(bits);
    unsigned char v;
    int c = 0;
    int len = length(bits);
    int i;
    for (i = 0; i < len; i++) {
        for (v = bytes[i]; v; c++) {
            v &= v - 1;  /* clear the least significant bit set */
        }
    }
    return ScalarInteger(c);
}

SEXP graph_bitarray_rowColPos(SEXP bits)
{
    SEXP ans, matDim, dimNames, colNames;
    int i, j = 0, k, len = length(bits), *indices,
        dim = asInteger(getAttrib(bits, install("bitdim"))), 
        edgeCount = asInteger(getAttrib(bits, install("nbitset")));
    unsigned char v, *bytes = (unsigned char *) RAW(bits);

    PROTECT(ans = allocVector(INTSXP, 2 * edgeCount));
    indices = INTEGER(ans);
    for (i = 0; i < len; i++) {
        for (v = bytes[i], k = 0; v; v >>= 1, k++) {
            if (v & 1) {
                int idx  = (i * 8) + k; 
                indices[j] =  (idx % dim) + 1; /* R is 1-based */
                indices[j + edgeCount] =  (idx / dim) + 1;
                j++;
            }
        }
    }
    PROTECT(matDim = allocVector(INTSXP, 2));
    INTEGER(matDim)[0] = edgeCount; INTEGER(matDim)[1] = 2;
    setAttrib(ans, R_DimSymbol, matDim);

    PROTECT(colNames = allocVector(STRSXP, 2));
    SET_STRING_ELT(colNames, 0, mkChar("from"));
    SET_STRING_ELT(colNames, 1, mkChar("to"));

    PROTECT(dimNames = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimNames, 0, R_NilValue);
    SET_VECTOR_ELT(dimNames, 1, colNames);
    setAttrib(ans, R_DimNamesSymbol, dimNames);
    UNPROTECT(4);
    return ans;
}


#define COORD_TO_INDEX(x, y, nrow) ((((y)+1) * (nrow)) - ((nrow) - ((x)+1)) - 1)
#define NROW(x) (INTEGER(getAttrib((x), install("bitdim")))[0])
#define INDEX_TO_ROW(i, n) ((i) % (n))
#define INDEX_TO_COL(i, n) ((i) / (n))
#define IS_SET(b, i, bit) ((b)[i] != 0 && ((b)[i] & (1 << (bit))))

SEXP graph_bitarray_transpose(SEXP bits)
{
    SEXP ans;
    int nrow, i, j, len = length(bits);
    unsigned char *bytes = RAW(bits), *ans_bytes;
    ans = PROTECT(duplicate(bits)); /* dup to capture attributes */
    ans_bytes = RAW(ans);
    memset(ans_bytes, 0, len);
    nrow = NROW(bits);
    /* FIXME: use a single loop, look at R's array.c */
    for (i = 0; i < nrow; i++) {
        for (j = 0; j < nrow; j++) {
            int idx = COORD_TO_INDEX(i, j, nrow),
                tidx = COORD_TO_INDEX(j, i, nrow);
            int byteIndex = idx / 8,
                bitIndex = idx % 8,
                tBitIndex = tidx % 8;
            if (IS_SET(bytes, byteIndex, bitIndex))
                ans_bytes[tidx / 8] |= (1 << tBitIndex);
        }
    }
    UNPROTECT(1);
    return ans;
}

/* Given a bit vector representing directed edges, return a new bit
   vector with the underlying undirected edges.
 */
SEXP graph_bitarray_undirect(SEXP bits)
{
    int i, j, c = 0, len = length(bits), nrow = NROW(bits);
    SEXP tbits = PROTECT(graph_bitarray_transpose(bits)),
         ans = PROTECT(duplicate(bits));
    unsigned char *bytes = RAW(bits), *tbytes = RAW(tbits), *abytes = RAW(ans);
    for (i = 0; i < len; i++) {
        unsigned char v;
        if (0 != (abytes[i] = bytes[i] | tbytes[i])) {
            /* keep track of edge count */
            for (v = abytes[i]; v; c++) {
                v &= v - 1;  /* clear the least significant bit set */
            }
        }
    }
    /* zero out lower tri */
    for (i = 0; i < nrow; i++) {
        for (j = 0; j < nrow; j++) {
            if (i > j) {
                unsigned char v;
                int idx = COORD_TO_INDEX(i, j, nrow);
                v = abytes[idx / 8];
                if (0 != v) {
                    if (IS_SET(abytes, idx / 8, idx % 8)) c--;
                    abytes[idx / 8] &= ~(1 << (idx % 8));
                }
            }
        }
    }
    INTEGER(getAttrib(ans, install("nbitset")))[0] = c;
    UNPROTECT(2);
    return ans;
}

SEXP graph_bitarray_set(SEXP bits, SEXP idx, SEXP val)
{
    SEXP ans = PROTECT(duplicate(bits));
    int *which, *values, i, nVal = length(val),
        *num_set = INTEGER(getAttrib(ans, install("nbitset")));
    unsigned char *bytes = RAW(ans);
    PROTECT(idx = coerceVector(idx, INTSXP));
    PROTECT(val = coerceVector(val, INTSXP));
    which = INTEGER(idx);
    values = INTEGER(val);
    for (i = 0; i < nVal; i++) {
        int w = which[i] - 1;
        int offset = w / 8;
        unsigned char bit = w % 8;
        if (values[i]) {
            if (!IS_SET(bytes, offset, bit)) (*num_set)++;
            bytes[offset] |= (1 << bit);
        } else {
            if (IS_SET(bytes, offset, bit)) (*num_set)--;
            bytes[offset] &= ~(1 << bit);
        }
    }
    UNPROTECT(3);
    return ans;
}

SEXP graph_bitarray_subGraph(SEXP bits, SEXP _subIndx) {
    
    SEXP _dim = getAttrib(bits,install("bitdim")),
        sgVec, btlen, btdim, btcnt, _ftSetPos, res, namesres;
    int dim, subLen, prevSetPos = 0, sgSetIndx = 0,
        linIndx = 0, col, subgBitLen, subgBytes,
        *subIndx, *ftSetPos, edgeCount = 0, ftLen = 256;
    PROTECT_INDEX pidx;
    unsigned char *bytes = (unsigned char *) RAW(bits), *sgBits;
    dim  = INTEGER(_dim)[0];
    subIndx = INTEGER(_subIndx);
    subLen = length(_subIndx);
    subgBitLen = subLen * subLen;
    subgBytes = subgBitLen / 8;
    if ((subgBitLen % 8) != 0) {
        subgBytes++;
    }
    PROTECT(sgVec = allocVector(RAWSXP, subgBytes));
    sgBits = RAW(sgVec);
    memset(sgBits, 0, subgBytes);
    /* TODO: in many cases, this will be more than we need, we should
       also use the number of edges in the input as a starting point.
    */
    _ftSetPos = allocVector(INTSXP, ftLen); /* FIXME: need better guess */
    PROTECT_WITH_INDEX(_ftSetPos, &pidx);
    ftSetPos = INTEGER(_ftSetPos); 
    for (col = 0; col < subLen; col++) { 
        int col_idx_dim = ((subIndx[col] - 1) * dim) - 1;
        int row = 0;
        while (row < subLen) {
            int setPos = col_idx_dim + subIndx[row];
            unsigned char v = bytes[setPos / 8];
            if (v != 0 && v & (1 << (setPos % 8))) {
                int curSetPos = setPos,
                    m = prevSetPos;
                while (m < curSetPos) {
                    unsigned char tempV = bytes[m / 8];
                    if (tempV == 0) {
                        m += 8;
                    } else {
                        if (tempV & (1 << (m % 8))) edgeCount++;
                        m++;
                    }
                }
                prevSetPos = curSetPos + 1;
                edgeCount++;    /* current edge */
                if (sgSetIndx == ftLen) {
                    ftLen *= 2;
                    if (ftLen > subgBitLen) ftLen = subgBitLen;
                    REPROTECT(_ftSetPos = lengthgets(_ftSetPos, ftLen), pidx);
                    ftSetPos = INTEGER(_ftSetPos);
                }
                ftSetPos[sgSetIndx] = edgeCount;
                sgSetIndx++;
                sgBits[linIndx / 8] |= (1 << (linIndx % 8));
            }
            linIndx++;
            row++;
        }
    }
    REPROTECT(_ftSetPos = lengthgets(_ftSetPos, sgSetIndx), pidx);
    PROTECT(btlen = ScalarInteger(subgBitLen));
    PROTECT(btcnt = ScalarInteger(sgSetIndx));
    PROTECT(btdim = allocVector(INTSXP, 2));
    INTEGER(btdim)[0] = subLen;
    INTEGER(btdim)[1] = subLen;
    setAttrib(sgVec, install("bitlen"), btlen);
    setAttrib(sgVec, install("bitdim"), btdim);
    setAttrib(sgVec, install("nbitset"), btcnt);
    PROTECT(res = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(res, 0, _ftSetPos);
    SET_VECTOR_ELT(res, 1, sgVec); 
    PROTECT(namesres = allocVector(STRSXP, 2));
    SET_STRING_ELT(namesres, 0, mkChar("setPos"));
    SET_STRING_ELT(namesres, 1, mkChar("bitVec"));
    setAttrib(res, R_NamesSymbol, namesres);
    UNPROTECT(7);
    return res;
}

SEXP graph_bitarray_edgeSetToMatrix(SEXP nodes, SEXP bits,
                                    SEXP _weights, SEXP _directed)
{
    SEXP ans, dnms, _dim = getAttrib(bits, install("bitdim"));
    unsigned char *bytes = (unsigned char *) RAW(bits);
    int dim = INTEGER(_dim)[0],
        num_el = dim * dim,
        directed = asInteger(_directed),
        linIndx = 0, wtIndx = 0;
    double *weights = REAL(_weights), *ftMat;
    PROTECT(ans = allocVector(REALSXP, num_el));
    ftMat = REAL(ans); 
    memset(ftMat, 0, sizeof(double) * num_el); 
    while (linIndx < num_el) {
        unsigned char v = bytes[linIndx / 8];
        if (v == 0) {
            linIndx += 8;
        } else {
            if (v & (1 << (linIndx % 8))) {
                ftMat[linIndx] = weights[wtIndx];
                if (!directed) {
                    ftMat[linIndx/dim + (linIndx % dim)*dim] = weights[wtIndx];
                }
                wtIndx++;
            }
            linIndx++;
        }
    }
    SET_NAMED(_dim, 2);
    setAttrib(ans, R_DimSymbol, _dim);
    PROTECT(dnms = allocVector(VECSXP, 2));
    /* Arguments to .Call have NAMED(x) == 2, so we can
       reuse here.
     */
    SET_VECTOR_ELT(dnms, 0, nodes);
    SET_VECTOR_ELT(dnms, 1, nodes);
    setAttrib(ans, R_DimNamesSymbol, dnms);
    UNPROTECT(2);
    return ans;    
}


SEXP graph_bitarray_getBitCell(SEXP bits, SEXP _from, SEXP _to)
{
    int len = length(_to);
    SEXP ans;
    PROTECT(ans = allocVector(LGLSXP, len));
    unsigned char *bytes = (unsigned char *) RAW(bits);
    int *from = INTEGER(_from);
    int *to = INTEGER(_to);
    int dim = NROW(bits);
    int i = 0, val, byteIndex, bitIndex, indx;
    for(i =0; i < len; i++) {
        indx = COORD_TO_INDEX(from[i]-1, to[i]-1, dim) ;
        byteIndex = indx / 8 ;
        bitIndex = indx % 8 ;
        val = bytes[byteIndex] & (1 << bitIndex); 
        LOGICAL(ans)[i] = 0;
        if (val) {
            LOGICAL(ans)[i] = 1;

        } 
    }
    UNPROTECT(1);
    return(ans);
}


SEXP graph_bitarray_Union_Attrs(SEXP inputBits, SEXP cmnBits, SEXP fromOneBits,
        SEXP fromTwoBits) {
    unsigned char *ans = (unsigned char*) RAW(inputBits);
    unsigned char *cmn = (unsigned char*) RAW(cmnBits);
    unsigned char *fromOne = (unsigned char *) RAW(fromOneBits);
    unsigned char *fromTwo = (unsigned char *) RAW(fromTwoBits);
    int len = length(inputBits) * 8;
    int i, byteIndex, bitIndex , shft, setIndx = 0;
    int nn = asInteger(getAttrib(inputBits, install("nbitset")));
    SEXP from, indx1, indx2 ;
    PROTECT(from = allocVector(INTSXP, nn));
    PROTECT(indx1 = allocVector(INTSXP , nn));
    PROTECT(indx2 = allocVector(INTSXP , nn));

    int from1Indx = 0;
    int from2Indx = 0;  
    int cmnIndx = 0;
    for( i =0 ; i < len; i ++) {
         byteIndex = i / 8;
         bitIndex = i % 8;
         shft = 1 << bitIndex;
         if(ans[byteIndex] & (shft)) {
            if(cmn [byteIndex] & (shft)) {
               cmnIndx++;
               from1Indx++;
               from2Indx++;
               INTEGER(from)[setIndx] = 0;
               INTEGER(indx1)[setIndx] = from1Indx ;
               INTEGER(indx2)[setIndx] = from2Indx;
            } else if(fromOne[byteIndex] & (shft)) {
               from1Indx++; 
               INTEGER(from)[setIndx] = 1;
               INTEGER(indx1)[setIndx] = from1Indx;
                      
            } else if(fromTwo[byteIndex] & (shft)) {
                from2Indx++;
                INTEGER(from)[setIndx] = 2;
                INTEGER(indx2)[setIndx] = from2Indx;
            }
            setIndx++;
         }
    }
     
    setAttrib(from, install("indx1"), indx1);
    setAttrib(from, install("indx2"), indx2);
    UNPROTECT(3);
    return(from);
}


SEXP graph_bitarray_Intersect_Attrs(SEXP cmnBits, SEXP fromOneBits, 
        SEXP fromTwoBits) {
    unsigned char *cmn = (unsigned char*) RAW(cmnBits);
    unsigned char *fromOne = (unsigned char *) RAW(fromOneBits);
    unsigned char *fromTwo = (unsigned char *) RAW(fromTwoBits);
    int len = length(cmnBits) * 8;
    int i, byteIndex, bitIndex , shft, setIndx = 0;
    int nn = asInteger(getAttrib(cmnBits, install("nbitset")));
    SEXP from, indx1, indx2;
    PROTECT(from = allocVector(INTSXP, nn));
    PROTECT(indx1 = allocVector(INTSXP , nn));
    PROTECT(indx2 = allocVector(INTSXP , nn));
    int from1Indx = 0;
    int from2Indx = 0;  
    for( i =0 ; i < len; i ++) {
         byteIndex = i / 8;
         bitIndex = i % 8;
         shft = 1 << bitIndex;
         if(fromOne[byteIndex] & (shft) ) {
               from1Indx++; 
         }
         if(fromTwo[byteIndex] & (shft)) {
                from2Indx++;
         }
         if(cmn[byteIndex] & (shft)) {
                 INTEGER(from)[setIndx] = 0;
                 INTEGER(indx1)[setIndx] = from1Indx;
                 INTEGER(indx2)[setIndx] = from2Indx;
                 setIndx++;
         } 
    }
    setAttrib(from, install("indx1"), indx1);
    setAttrib(from, install("indx2"), indx2);
    UNPROTECT(3);
    return(from);
}


SEXP graph_bitarray_removeEdges(SEXP bits, SEXP _indx) {
    
    SEXP ans = PROTECT(duplicate(bits)), btcnt;
    unsigned char *bytes = (unsigned char *) RAW(ans);
    int *indx =  INTEGER(_indx);
    int len = length(bits) * 8 ;
    int i, byteIndex, bitIndex, subIndx =0;
    int nSet = 0;
    unsigned char mask;
    for( i =0 ; i < len; i ++) {
         byteIndex = i / 8;
         bitIndex = i % 8;
         if(IS_SET(bytes, byteIndex, bitIndex)) {
           if(indx[subIndx] == 0){
               mask = ~(1 << bitIndex) ;
               bytes[byteIndex] = bytes[byteIndex] & mask;
           } else {
                nSet++;
           }
            subIndx++;
         }         
    }
    PROTECT(btcnt = ScalarInteger(nSet));
    setAttrib(ans, install("nbitset"), btcnt);
    UNPROTECT(2);
    return(ans);
}

SEXP graph_bitarray_getEdgeAttrOrder(SEXP _bits, SEXP _from, SEXP _to) {
    unsigned char *bits = (unsigned char*) RAW(_bits);
    int ns = asInteger(getAttrib(_bits, install("nbitset")));
    int len = length(_from);
    int *from = INTEGER(_from);
    int *to = INTEGER(_to);
    int dim = NROW(_bits);
    int byteIndex, bitIndex, shft, indx, intIndx, i, j;
    int oindx=0, nindx=0, attrIndx=0, setCount=0;
    SEXP origRightPos, origLeftPos, newRightPos, newLeftPos, res, namesres;
    PROTECT(origRightPos = allocVector(INTSXP, ns)); //index into orig attr
    PROTECT(origLeftPos = allocVector(INTSXP, ns));
    PROTECT(newRightPos = allocVector(INTSXP, len));
    PROTECT(newLeftPos = allocVector(INTSXP, len));

    setCount =1;
    for(j =0; j < dim ; j ++) {
        for(i =0; i < dim; i++){
            indx =  COORD_TO_INDEX(i, j , dim);
            byteIndex = indx / 8;
            bitIndex = indx % 8;
            shft = 1 << bitIndex;
            intIndx = COORD_TO_INDEX(from[attrIndx]-1, to[attrIndx]-1, dim);
            if(bits[byteIndex] & (shft) ) {
                INTEGER(origRightPos)[oindx]  = oindx + 1  ;
                INTEGER(origLeftPos)[oindx] = setCount    ;
                oindx++;
                if(intIndx != indx){
                   setCount++; 
                }
            }
            if(intIndx == indx) {
               INTEGER(newRightPos)[nindx] =  nindx + 1;
               INTEGER(newLeftPos)[nindx] = setCount  ;
               nindx++;
               if(attrIndx < len-1){
                   attrIndx++; 
               }  
               setCount++;
            }
        }
    }
    SET_LENGTH(newRightPos, nindx);
    SET_LENGTH(newLeftPos, nindx);
    
    PROTECT(res = allocVector(VECSXP, 4));
    SET_VECTOR_ELT(res, 0, newLeftPos);
    SET_VECTOR_ELT(res, 1, newRightPos); 
    SET_VECTOR_ELT(res, 2, origLeftPos); 
    SET_VECTOR_ELT(res, 3, origRightPos); 
    PROTECT(namesres = allocVector(STRSXP, 4));
    SET_STRING_ELT(namesres, 0, mkChar("newLeftPos"));
    SET_STRING_ELT(namesres, 1, mkChar("newRightPos"));
    SET_STRING_ELT(namesres, 2, mkChar("origLeftPos"));
    SET_STRING_ELT(namesres, 3, mkChar("origRightPos"));
    setAttrib(res, R_NamesSymbol, namesres);
    UNPROTECT(6);
    return(res);

}

