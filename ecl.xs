#include <ecl/ecl.h>

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"


#define PACKAGE_LIST      "ecl::List"
#define PACKAGE_CODE      "ecl::Code"
#define PACKAGE_CHAR      "ecl::Char"
#define PACKAGE_RATIO     "ecl::Ratio"
#define PACKAGE_BIGNUM    "ecl::Bignum"
#define PACKAGE_SYMBOL    "ecl::Symbol"
#define PACKAGE_STRING    "ecl::String"
#define PACKAGE_GENERIC   "ecl::Generic"
#define PACKAGE_PACKAGE   "ecl::Package"
#define PACKAGE_HASHTABLE "ecl::HashTable"

#if defined(WIN32)
#      define EXP __declspec(dllexport)
#else
#      define EXP
#endif

static int boot_done = 0;
static cl_object current_package = 0;

static PerlInterpreter *_my_perl = 0;

/* the structure below is used to pass SvPV to LISP */
/* non-threadsafe usage! */
static struct ecl_base_string lisp_str = {
    t_base_string, 0, 0, 0,
    Cnil,
    0,
    0,
    0
};

/* function to aid evaling perl from lisp code; connected fia FFI */
static char*
_eval_perl_(char *s) {
    fprintf(stderr, "_eval_perl, yep!\n");
    return "";
}
/*
(cffi:defcfun "eval_wrapper" :av
  (code :string))
(cffi:defcfun "call_wrapper" :av
  (fun :sv)
  (args :sv))
*/
EXP SV* eval_wrapper(SV *code)
{
    dTHX; /* fetch context */
    dSP;
    I32 count, i;
    SV *sv;
    SV *rc = &PL_sv_undef;

    ENTER;
    SAVETMPS;

    PUSHMARK(sp);
    PUTBACK;
    count = perl_eval_sv(code, G_EVAL|G_ARRAY);
	//fprintf(stderr,"count=%d;\n",count);
    SPAGAIN;

    if (SvTRUE(ERRSV)) {
	/* signal error... */
	//fprintf(stderr,"err in eval!\n");
	POPs; /* pop the undef off the stack */
    }
    else {
	AV *av = newAV();
	av_extend(av,count);
	for (i = 0; i < count; i++) {
	    sv = POPs; /* pop value off the stack */
	    SvREFCNT_inc(sv);  /*this makes leakage???*/
	    av_store(av, count-i-1, sv);
	}
	//rc = sv_bless(newRV_noinc((SV *) av), gv_stashpv("Lisp::List", 1));
	rc = av;
    }

    PUTBACK;
    FREETMPS;
    LEAVE;
    return rc;
}
EXP SV* call_wrapper(SV *fun, AV *args)
{
    dTHX; /* fetch context */
    dSP;
    I32 count, i=0;
    SV *sv, **ssv;
    SV *rc = &PL_sv_undef;

    ENTER;
    SAVETMPS;

    PUSHMARK(sp);
    count = av_len(args);
    for (i=0; i< count; i++) {
	ssv = av_fetch(args,i,0);
	PUSHs(*ssv);
    }
    PUTBACK;
    count = perl_call_sv(fun, G_EVAL|G_ARRAY);
	//fprintf(stderr,"(call)count=%d;\n",count);
    SPAGAIN;

    if (SvTRUE(ERRSV)) {
	/* signal error... */
	//fprintf(stderr,"(call)err in eval!\n");
	POPs; /* pop the undef off the stack */
    }
    else {
	AV *av = newAV();
	//av_extend(av,count);
	for (i = 0; i < count; i++) {
	    sv = POPs; /* pop value off the stack */
	    SvREFCNT_inc(sv);  /*this makes leakage???*/
	    av_store(av, count-i-1, sv);
	}
	//rc = sv_bless(newRV_noinc((SV *) av), gv_stashpv("Lisp::List", 1));
	rc = av;
    }

    PUTBACK;
    FREETMPS;
    LEAVE;
    return rc;
}

static HV*
create_lisp_on_sv(SV *rsv, const char *pkg_name)
{
    char classname[200];
    SV *sv = SvRV(rsv);
    HV *phv;
    if (strchr(pkg_name,':')!=0) {
	strcpy(classname,pkg_name); /* if full name given, use it */
    } else {
	sprintf(classname, "ecl::%s",pkg_name); /*otherwise ...*/
    }
    phv = gv_stashpv(classname,GV_ADD);
    sv_bless(rsv,phv);
    return phv;
}

static SV*
create_lisp_sv(pTHX_ const char *pkg_name, cl_object obj)
{
    SV *sv = newSVpv((char*)&obj,4);
    SV *rsv = newRV_noinc(sv); /* after this sv will have refcnt 1 (fortunately) */
    create_lisp_on_sv(rsv, pkg_name);
    return rsv;
}

static SV*
create_lisp_av(pTHX_ const char *pkg_name, cl_object obj)
{
    SV *sv = newSVpv((char*)&obj,4);
    AV *av = newAV();
    SV *rsv = newRV_noinc(sv); /* after this sv will have refcnt 1 (fortunately) */
    SV *rav = newRV_noinc(av); /* after this sv will have refcnt 1 (fortunately) */
    HV *phv = create_lisp_on_sv(rsv, pkg_name);
    /* have blessed reference, now TIE it! */
    sv_magic(rsv,phv,PERL_MAGIC_tied,0,0);
    return rav;
}

/* given blessed reference, which is actually pointer to cl_object,
 * return this as lisp object */
static cl_object
sv2cl_nocheck(pTHX_ SV *sv)
{
    cl_object clo;
    /* proper checks are not within scope of this fun */
    SV *sv_deref = SvRV(sv);
    memcpy(&clo,SvPV_nolen(sv_deref),4);
    return clo;
}

static cl_object
sv2cl(SV *sv)
{
    if (sv_isobject(sv)) {
	if (sv_isobject(sv) && sv_derived_from(sv, "ecl")) {
	    cl_object clo;
	    SV *sv_deref = SvRV(sv);
	    memcpy(&clo,SvPV_nolen(sv_deref),4);
	    return clo;
	} else {
	    SV *sv_deref = SvRV(sv);
	    fprintf(stderr,"sv2cl: str=%s;\n",SvPV_nolen(sv_deref));
	    croak("sv2cl: passed not a subclass of ecl");
	}
    } else if (SvIOK(sv)) {
	int iv = SvIV(sv); 
        /* fprintf(stderr,"SvIOK, %d, good!\n",iv); */
	return ecl_make_integer(iv);
    } else if (SvPOK(sv)) {
	int len;
        char *str = SvPV(sv,len);
        cl_object x = cl_alloc_simple_base_string(len);
        /* fprintf(stderr,"SvPOK, %s, good!\n",str); */
        memcpy(x->base_string.self, str, len);
        /*x->base_string.self[len] = 0;*/ // TODO freeing of this stuff
	return x;
    } else {
	fprintf(stderr,"sv2cl: str=%s;\n",SvPV_nolen(sv));
	croak("sv2cl: passed not a subclass of ecl, not string and not int");
    }
}

static SV *
cl2sv(pTHX_ cl_object clo)
{
    SV *sv;
    char *h;
    int i;
    switch (type_of(clo)) {
    case t_character:
	sv = create_lisp_sv(aTHX_ PACKAGE_CHAR, clo);
        break;
    case t_bignum:
	sv = create_lisp_sv(aTHX_ PACKAGE_BIGNUM, clo);
        break;
    case t_ratio:
	sv = create_lisp_sv(aTHX_ PACKAGE_RATIO, clo);
        break;
    case t_list:
	sv = create_lisp_sv(aTHX_ PACKAGE_LIST, clo);
	break;
    case t_fixnum:
	sv = newSViv(fix(clo));
	break;
    case t_base_string:
	sv = newSVpv(clo->base_string.self,clo->base_string.fillp);
	break;
#ifdef ECL_UNICODE
    case t_string:
	sv = newSVpvn("",clo->string.fillp);
        h = SvPV_nolen(sv);
	/*fprintf(stderr,"debug - t_string(%d);s=%s h=%08X\n",clo->string.fillp,clo->string.self,h);*/
	for (i = 0; i < clo->string.fillp; i++) {
	    if (!ECL_BASE_CHAR_CODE_P(clo->string.self[i]))
	        croak("bug: t_string");
	    h[i] = clo->string.self[i];
	}
	break;
#endif
    case t_package:
	sv = create_lisp_sv(aTHX_ PACKAGE_PACKAGE, clo);
	break;
    case t_hashtable:
	sv = create_lisp_sv(aTHX_ PACKAGE_HASHTABLE, clo);
	break;
    case t_bytecodes:
	sv = create_lisp_sv(aTHX_ PACKAGE_CODE, clo);
	break;
    case t_symbol:
	    /*
	n = ecl_symbol_name(clo);
	p = ecl_symbol_package(clo);
	fprintf(stderr,"n-type %d, n=%s p-type %d\n",
	    type_of(n),
	    (type_of(n)==t_base_string?n->base_string.self:"??"),
	    type_of(p));
	    */
	sv = create_lisp_sv(aTHX_ PACKAGE_SYMBOL, clo);
	break;
    default:
	fprintf(stderr,"type %d not impl!\n",type_of(clo));
	sv = create_lisp_sv(aTHX_ PACKAGE_GENERIC, clo);
	break;
    }
    return sv;
}

/* returns cl_object which is surely t_basestring */
static cl_object
generic_stringify(cl_object clo) {
    cl_object o, strm = ecl_make_string_output_stream(0,0);
    /*fprintf(stderr,"generic_stringify\n");*/
    si_write_object(clo,strm);
    o = cl_get_output_stream_string(strm);
    ecl_dealloc(strm);
    if (type_of(o) != t_base_string) {
	croak("bug: type_of(o) != t_basestring!");
    }
    return o;
}

static void
free_cl(SV *sv)
{
    cl_object o = sv2cl(sv);
    if (type_of(o) == t_base_string) {
	fprintf(stderr,"freeing a base_string\n");
        GC_free(o->base_string.self);
    } else {
	fprintf(stderr,"free of type %d not impl!\n",type_of(o));
    }
}

MODULE = ecl::List		PACKAGE = ecl::List		

SV *
FETCH(this, n)
        SV *this
	int n
    PREINIT:
        cl_object clo = sv2cl(this);
    CODE:
	/* get n-th item, obviously... */
	/* fprintf(stderr,"FETCH(this) n=%d, len=%d\n", n, len); */
	if (type_of(clo) == t_list) {
	    cl_object o;
	    if (n<0) {
		int len = fix(cl_list_length(clo));
	        n = len+n;
	    }
	    if (n<0) {
		RETVAL = &PL_sv_undef;
	    }
	    o = ecl_nth(n,clo);
	    if (o==Cnil || o==OBJNULL) {
		RETVAL = &PL_sv_undef;
	    } else {
		RETVAL = cl2sv(aTHX_ o);
	    }
	} else {
	    croak("weird lisp object, must be t_list");
	}
    OUTPUT:
    	RETVAL

void
STORE(this, n, val)
        SV *this
	int n
        SV *val
    PREINIT:
        cl_object clo = sv2cl(this);
        cl_object clval = sv2cl(val);
	int len = fix(cl_list_length(clo));
    CODE:
	/* set n-th item, NOT IMPLEMENTED... */
	if (type_of(clo) == t_list) {
	    if (n<0) {
	        n = len+n;
	    }
	    if (n<0) {
		return;
	    }
	    if (n>=len || n<0) {
	    }
	    croak("STORE not here");
	} else {
	    croak("weird lisp object, must be t_list");
	}

int
FETCHSIZE(this)
        SV *this
    PREINIT:
        cl_object clo = sv2cl(this);
	int len = fix(cl_list_length(clo));
    CODE:
	/* get number of items, obviously... */
	if (type_of(clo) == t_list) {
	    RETVAL = len;
	} else {
	    croak("weird lisp object, must be t_list");
	}
    OUTPUT:
    	RETVAL

SV *
TIEARRAY(classname, obj)
        SV *classname
	SV *obj
    PREINIT:
	HV *phv = gv_stashsv(classname,GV_ADD);
    CODE:
        if (sv_isobject(obj)) {
	    /* if its object then blessing could lose
	     * refcount on it ?? */
	    SvREFCNT_inc(obj);
	    RETVAL = sv_bless(obj,phv);
	    /* SvREFCNT_dec(obj); this not work */
	} else {
	    RETVAL = sv_bless(obj,phv);
	}
    OUTPUT:
    	RETVAL

MODULE = ecl::Package		PACKAGE = ecl::Package		

SV *
stringify(clsv, ...)
        SV *clsv
    PREINIT:
        cl_object clo, np;
    CODE:
        clo = sv2cl(clsv);
	if (type_of(clo) == t_package) {
	    np = cl_package_name(clo);
	    RETVAL = newSVpvf("#<PACKAGE %s>",
		(type_of(np)==t_base_string?np->base_string.self:"??")
	      );
	} else {
	    croak("can not stringify non-t_package within ...::Package package");
	}
    OUTPUT:
    	RETVAL

MODULE = ecl::Symbol		PACKAGE = ecl::Symbol		

SV *
stringify(clsv, ...)
        SV *clsv
    PREINIT:
        cl_object clo, n, p, np;
    CODE:
        clo = sv2cl(clsv);
	if (type_of(clo) == t_symbol) {
	    n = ecl_symbol_name(clo);
	    p = ecl_symbol_package(clo);
	    np = cl_package_name(p);
	    RETVAL = newSVpvf("#<SYMBOL %s::%s>",
		(type_of(np)==t_base_string?np->base_string.self:"??"),
		(type_of(n)==t_base_string?n->base_string.self:"??")
	      );
	} else {
	    croak("can not stringify non-t_symbol within ...::Symbol package");
	}
    OUTPUT:
    	RETVAL

MODULE = ecl::Char		PACKAGE = ecl::Char		

SV *
stringify(clsv, ...)
        SV *clsv
    PREINIT:
        cl_object clo;
	int ccode;
    CODE:
        clo = sv2cl(clsv);
	if (type_of(clo) == t_character) {
	    ccode = CHAR_CODE(clo);
	    RETVAL = newSVpvf("%c",ccode); /*TBD improve here*/
	} else {
	    croak("can not stringify non-t_character within ...::Char package");
	}
    OUTPUT:
    	RETVAL

MODULE = ecl::Bignum		PACKAGE = ecl::Bignum		

SV *
stringify0(clsv)
        SV *clsv
    PREINIT:
        cl_object clo, o;
    CODE:
        clo = sv2cl(clsv);
	o = generic_stringify(clo);
	RETVAL = newSVpvn(o->base_string.self,o->base_string.fillp);
	ecl_dealloc(o);
    OUTPUT:
    	RETVAL

SV *
stringify(clsv, ...)
        SV *clsv
    PREINIT:
        cl_object clo;
    CODE:
        clo = sv2cl(clsv);
	if (type_of(clo) == t_bignum) {
	    cl_object o = generic_stringify(clo);
	    /* should use length of string also? */
	    RETVAL = newSVpvf("#<BIGNUM %s>", o->base_string.self);
	    ecl_dealloc(o);
	} else {
	    croak("can not stringify non-t_bignum within ...::Bignum package");
	}
    OUTPUT:
    	RETVAL

MODULE = ecl::Ratio		PACKAGE = ecl::Ratio		

SV *
stringify(clsv, ...)
        SV *clsv
    PREINIT:
        cl_object clo;
    CODE:
	/* fprintf(stderr,"ratio stringify\n"); */
        clo = sv2cl(clsv);
	if (type_of(clo) == t_ratio) {
	    SV *den = cl2sv(aTHX_ clo->ratio.den);
	    SV *num = cl2sv(aTHX_ clo->ratio.num);
	    SV *denss, *numss;
	    char *denstr, *numstr;

	    if (sv_isobject(den)) {
		PUSHMARK(SP);
		XPUSHs(den);
		PUTBACK;
		call_method("stringify0",G_SCALAR);
                SPAGAIN;
                denss = POPs;
                PUTBACK;
	    } else {
		denss = den;
	    }
	    denstr = SvPV_nolen(denss);

	    if (sv_isobject(num)) {
		PUSHMARK(SP);
		XPUSHs(num);
		PUTBACK;
		call_method("stringify0",G_SCALAR);
                SPAGAIN;
                numss = POPs;
                PUTBACK;
	    } else {
		numss = num;
	    }
	    numstr = SvPV_nolen(numss);

	    RETVAL = newSVpvf("#<RATIO %s/%s>", numstr, denstr);
	} else {
	    croak("can not stringify non-t_ratio within ...::Ratio package");
	}
    OUTPUT:
    	RETVAL

MODULE = ecl::HashTable		PACKAGE = ecl::HashTable		

SV *
stringify(clsv, ...)
        SV *clsv
    PREINIT:
        cl_object clo;
    CODE:
        clo = sv2cl(clsv);
	if (type_of(clo) == t_hashtable) {
	    RETVAL = newSVpvf("#S<HASH-TABLE>");
	} else {
	    croak("can not stringify non-t_hashtable within ...::HashTable package");
	}
    OUTPUT:
    	RETVAL

int
EXISTS(this, key)
        SV *this
	SV *key
    PREINIT:
        cl_object clo = sv2cl(this);
        cl_object k = sv2cl(key);
	struct ecl_hashtable_entry *he;
    CODE:
	/* get 'key' item, obviously... */
	if (type_of(clo) == t_hashtable) {
	    croak("NYI - please ask author to re-code it in ecl.xs!!!");
           /*
	    he = ecl_search_hash(k,clo);
	    if (he==Cnil || he==OBJNULL) {
		RETVAL = 0;
	    } else {
		RETVAL = 1;
	    }*/
	} else {
	    croak("weird lisp object, must be t_hashtable");
	}
    OUTPUT:
    	RETVAL

SV *
FETCH(this, key)
        SV *this
	SV *key
    PREINIT:
        cl_object clo = sv2cl(this);
        cl_object k = sv2cl(key);
    CODE:
	/* get 'key' item, obviously... */
	if (type_of(clo) == t_hashtable) {
	    cl_object o = ecl_gethash(k,clo);
	    if (o==Cnil || o==OBJNULL) {
		RETVAL = &PL_sv_undef;
	    } else {
		RETVAL = cl2sv(aTHX_ o);
	    }
	} else {
	    croak("weird lisp object, must be t_hashtable");
	}
    OUTPUT:
    	RETVAL

void
STORE(this, key, val)
        SV *this
	SV *key
        SV *val
    PREINIT:
        cl_object clo = sv2cl(this);
        cl_object clval = sv2cl(val);
        cl_object k = sv2cl(key);
    CODE:
	/* store item */
	if (type_of(clo) == t_hashtable) {
	    ecl_sethash(k,clo,clval);
	} else {
	    croak("weird lisp object, must be t_hashtable");
	}

SV *
TIEHASH(classname, obj)
        SV *classname
	SV *obj
    PREINIT:
	HV *phv = gv_stashsv(classname,GV_ADD);
    CODE:
        if (sv_isobject(obj)) {
	    /* if its object then blessing could lose
	     * refcount on it ?? */
	    SvREFCNT_inc(obj);
	    RETVAL = sv_bless(obj,phv);
	    /* SvREFCNT_dec(obj); this not work */
	} else {
	    RETVAL = sv_bless(obj,phv);
	}
    OUTPUT:
    	RETVAL

MODULE = ecl::Code		PACKAGE = ecl::Code		

SV *
funcall(self, ...)
	SV *self
    PREINIT:
	cl_object def = sv2cl(self);
	cl_object res, args[10];
	int items1 = items;
    CODE:
        switch (items1) {
	case 10:
	    args[8] = sv2cl(ST(9));
	    args[7] = sv2cl(ST(8));
	    args[6] = sv2cl(ST(7));
	    args[5] = sv2cl(ST(6));
	    args[4] = sv2cl(ST(5));
	    args[3] = sv2cl(ST(4));
	    args[2] = sv2cl(ST(3));
	    args[1] = sv2cl(ST(2));
	    args[0] = sv2cl(ST(1));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 9:
	    args[7] = sv2cl(ST(8));
	    args[6] = sv2cl(ST(7));
	    args[5] = sv2cl(ST(6));
	    args[4] = sv2cl(ST(5));
	    args[3] = sv2cl(ST(4));
	    args[2] = sv2cl(ST(3));
	    args[1] = sv2cl(ST(2));
	    args[0] = sv2cl(ST(1));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 8:
	    args[6] = sv2cl(ST(7));
	    args[5] = sv2cl(ST(6));
	    args[4] = sv2cl(ST(5));
	    args[3] = sv2cl(ST(4));
	    args[2] = sv2cl(ST(3));
	    args[1] = sv2cl(ST(2));
	    args[0] = sv2cl(ST(1));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3],args[4],args[5],args[6]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 7:
	    args[5] = sv2cl(ST(6));
	    args[4] = sv2cl(ST(5));
	    args[3] = sv2cl(ST(4));
	    args[2] = sv2cl(ST(3));
	    args[1] = sv2cl(ST(2));
	    args[0] = sv2cl(ST(1));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3],args[4],args[5]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 6:
	    args[4] = sv2cl(ST(5));
	    args[3] = sv2cl(ST(4));
	    args[2] = sv2cl(ST(3));
	    args[1] = sv2cl(ST(2));
	    args[0] = sv2cl(ST(1));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3],args[4]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 5:
	    args[3] = sv2cl(ST(4));
	    args[2] = sv2cl(ST(3));
	    args[1] = sv2cl(ST(2));
	    args[0] = sv2cl(ST(1));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 4:
	    args[2] = sv2cl(ST(3));
	    args[1] = sv2cl(ST(2));
	    args[0] = sv2cl(ST(1));
	    res = cl_funcall(items1,def,args[0],args[1],args[2]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 3:
	    args[1] = sv2cl(ST(2));
	    args[0] = sv2cl(ST(1));
	    res = cl_funcall(items1,def,args[0],args[1]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 2:
	    args[0] = sv2cl(ST(1));
	    res = cl_funcall(items1,def,args[0]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 1:
	    res = cl_funcall(items1,def);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	default:
	    croak("items %d not supported - wtf", items);
	}
    OUTPUT:
    	RETVAL

MODULE = ecl::String		PACKAGE = ecl::String		

SV *
stringify(clsv, ...)
        SV *clsv
    PREINIT:
        cl_object clo;
    CODE:
        clo = sv2cl(clsv);
	if (type_of(clo) == t_base_string) {
	    RETVAL = newSVpv(clo->base_string.self,clo->base_string.fillp);
	} else {
	    croak("can not stringify non-t_base_string within ...::String package");
	}
    OUTPUT:
    	RETVAL

MODULE = ecl::Generic		PACKAGE = ecl::Generic		

SV *
stringify(clsv, ...)
        SV *clsv
    PREINIT:
	char *types[] = {
	"t_start",
	"t_list",
	"t_character",
	"t_fixnum",
#ifdef ECL_SHORT_FLOAT
	"t_shortfloat",
#endif
	"t_bignum",
	"t_ratio",
	"t_singlefloat",
	"t_doublefloat",
#ifdef ECL_LONG_FLOAT
	"t_longfloat",
#endif
	"t_complex",
	"t_symbol",
	"t_package",
	"t_hashtable",
	"t_array",
	"t_vector",
#ifdef ECL_UNICODE
	"t_string",
#endif
	"t_base_string",
	"t_bitvector",
	"t_stream",
	"t_random",
	"t_readtable",
	"t_pathname",
	"t_bytecodes",
	"t_cfun",
	"t_cclosure",
#ifdef CLOS
	"t_instance",
#else
	"t_structure",
#endif /* CLOS */
#ifdef ECL_THREADS
	"t_process",
	"t_lock",
	"t_condition_variable",
#endif
	"t_codeblock",
	"t_foreign",
	"t_frame",
	"t_end",
	"t_other",
	"t_contiguous"
	    /*FREE = 127 */      /*  free object  */
	};
	int t;
	char *h;
	cl_object o = sv2cl(clsv);
    CODE:
	t = type_of(o);
	if (t==127) {h="FREE";}
	else {h=types[t];}
	RETVAL = newSVpvf("can not stringify %s within ...::Generic package", h);
    OUTPUT:
    	RETVAL

MODULE = ecl		PACKAGE = ecl		

int
cl_boot()
    PREINIT:
        char *argv1[] = {""};
	char buf[256];
	cl_object def;
	cl_object res;
    CODE:
	//argc, argv TODO int argc
	//argc, argv TODO char **argv
        RETVAL = cl_boot(0,argv1);
	current_package = ecl_current_package();
        _my_perl = my_perl;
        /* also create/init here perl::ev function */
        /* (or do it in BOOT section ? - to be decided l8r) */
        /**/
        /* declare addr of eval helper */
        //t = _eval_perl_;
        sprintf(buf,"(defvar *_ev_perl_* (ffi:make-pointer #X%08X :void))", _eval_perl_);
        def = c_string_to_object(buf);
	////////res = si_safe_eval(3,def,Cnil,OBJNULL);
        //
        //printf("buf=%s hehe;\n", buf);

	//cl_object def = c_string_to_object("(ffi:def-function)");
	//res = si_safe_eval(3,def,Cnil,OBJNULL);
        //_eval("");
        /**/
	boot_done = 1;
    OUTPUT:
    	RETVAL

SV *
cl_boot1()
    PREINIT:
	char buf[256];
	cl_object def;
	cl_object res;
    CODE:
        sprintf(buf,"(defvar *_ev_perl_* (ffi:make-pointer #X%08X :void)    )", _eval_perl_);
        printf("buf=%s hehe;\n", buf);
        def = c_string_to_object(buf);
	res = si_safe_eval(3,def,Cnil,OBJNULL);
        printf("buf=%s hehe;\n", buf);
	RETVAL = (res?cl2sv(aTHX_ res):&PL_sv_undef);
    OUTPUT:
    	RETVAL

void
cl_shutdown()
    CODE:
        cl_shutdown();

SV *
_eval(s)
	char *s
    PREINIT:
	cl_object def;
	cl_object res;
    CODE:
	//if (!boot_done)
	//    XS_Language__Lisp__ECL_cl_boot(aTHX);
        def = c_string_to_object(s);
	res = si_safe_eval(3,def,Cnil,OBJNULL);
	/* destroy def (not work!!! TODO) */
	/* ecl_dealloc(def->base_string.self); */
	/* def->base_string.self = 0; */
	/* ecl_dealloc(def); */
	/* (check for memory leaks?) */
	RETVAL = (res?cl2sv(aTHX_ res):&PL_sv_undef);
    OUTPUT:
    	RETVAL


SV *
_eval_form(lispobj)
	SV *lispobj
    PREINIT:
	cl_object def = sv2cl(lispobj);
	cl_object res;
    CODE:
	/* res = cl_eval(def); */
	res = si_safe_eval(3,def,Cnil,OBJNULL);
	RETVAL = (res?cl2sv(aTHX_ res):&PL_sv_undef);
    OUTPUT:
    	RETVAL

SV *
funcall(self,lispobj, ...)
	SV *self
	SV *lispobj
    PREINIT:
	cl_object def = sv2cl(lispobj);
	cl_object res, args[10];
	int items1 = items-1;
    CODE:
        switch (items1) {
	case 10:
	    args[8] = sv2cl(ST(10));
	    args[7] = sv2cl(ST(9));
	    args[6] = sv2cl(ST(8));
	    args[5] = sv2cl(ST(7));
	    args[4] = sv2cl(ST(6));
	    args[3] = sv2cl(ST(5));
	    args[2] = sv2cl(ST(4));
	    args[1] = sv2cl(ST(3));
	    args[0] = sv2cl(ST(2));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 9:
	    args[7] = sv2cl(ST(9));
	    args[6] = sv2cl(ST(8));
	    args[5] = sv2cl(ST(7));
	    args[4] = sv2cl(ST(6));
	    args[3] = sv2cl(ST(5));
	    args[2] = sv2cl(ST(4));
	    args[1] = sv2cl(ST(3));
	    args[0] = sv2cl(ST(2));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 8:
	    args[6] = sv2cl(ST(8));
	    args[5] = sv2cl(ST(7));
	    args[4] = sv2cl(ST(6));
	    args[3] = sv2cl(ST(5));
	    args[2] = sv2cl(ST(4));
	    args[1] = sv2cl(ST(3));
	    args[0] = sv2cl(ST(2));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3],args[4],args[5],args[6]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 7:
	    args[5] = sv2cl(ST(7));
	    args[4] = sv2cl(ST(6));
	    args[3] = sv2cl(ST(5));
	    args[2] = sv2cl(ST(4));
	    args[1] = sv2cl(ST(3));
	    args[0] = sv2cl(ST(2));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3],args[4],args[5]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 6:
	    args[4] = sv2cl(ST(6));
	    args[3] = sv2cl(ST(5));
	    args[2] = sv2cl(ST(4));
	    args[1] = sv2cl(ST(3));
	    args[0] = sv2cl(ST(2));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3],args[4]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 5:
	    args[3] = sv2cl(ST(5));
	    args[2] = sv2cl(ST(4));
	    args[1] = sv2cl(ST(3));
	    args[0] = sv2cl(ST(2));
	    res = cl_funcall(items1,def,args[0],args[1],args[2],args[3]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 4:
	    args[2] = sv2cl(ST(4));
	    args[1] = sv2cl(ST(3));
	    args[0] = sv2cl(ST(2));
	    res = cl_funcall(items1,def,args[0],args[1],args[2]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 3:
	    args[1] = sv2cl(ST(3));
	    args[0] = sv2cl(ST(2));
	    res = cl_funcall(items1,def,args[0],args[1]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 2:
	    args[0] = sv2cl(ST(2));
	    res = cl_funcall(items1,def,args[0]);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	case 1:
	    res = cl_funcall(items1,def);
	    RETVAL = cl2sv(aTHX_ res);
	    break;
	default:
	    croak("nitems %d not supported - wtf", items);
	}
    OUTPUT:
    	RETVAL


SV *
_search_lisp_function(fname)
	SV *fname
    PREINIT:
        int len, intern = 0;
        cl_object fun;
        cl_object sym;
    CODE:
	lisp_str.self = SvPV(fname,len);
	lisp_str.dim = len;
	lisp_str.fillp = len;
	sym = ecl_find_symbol((cl_object)&lisp_str, current_package, &intern);
        fun = ecl_fdefinition(sym);
	if (fun==OBJNULL || fun==Cnil) {
	    /* fprintf(stderr,"(fun==OBJNULL) %08X\n", fun); */
	    RETVAL = &PL_sv_undef;
	} else {
	    /* found function definition, so blessed object to ...::Code package
	     * is returned */
	    RETVAL = create_lisp_sv(aTHX_ PACKAGE_CODE,fun);
	}
    OUTPUT:
    	RETVAL


SV *
_keyword(keyw)
	const char *keyw
    PREINIT:
        cl_object sym = ecl_make_keyword(keyw);
    CODE:
	RETVAL = create_lisp_sv(aTHX_ PACKAGE_SYMBOL,sym);
    OUTPUT:
    	RETVAL

SV *
keyword(self, keyw)
	SV *self
	const char *keyw
    PREINIT:
        cl_object sym = ecl_make_keyword(keyw);
    CODE:
	RETVAL = create_lisp_sv(aTHX_ PACKAGE_SYMBOL,sym);
    OUTPUT:
    	RETVAL

SV *
_char(chr)
	SV *chr
    PREINIT:
        cl_object ch;
	int ccode;
    CODE:
	if (SvIOK(chr)) {
	    ccode = SvIV(chr); 
	} else if (SvPOK(chr)) {
	    int len;
	    char *str = SvPV(chr,len);
	    if (len!=1) {
		croak("pers rep of lisp char must be either int or string of length 1");
	    }
	    ccode = str[0]; /* unicode TBD */
	} else {
	    croak("pers rep of lisp char must be either int or string of length 1");
	}
        ch = CODE_CHAR(ccode);
	RETVAL = create_lisp_sv(aTHX_ PACKAGE_CHAR,ch);
    OUTPUT:
    	RETVAL

SV *
_s(sname)
	SV *sname
    PREINIT:
        int len, intern = 0;
        cl_object sym;
    CODE:
	lisp_str.self = SvPV(sname,len);
	lisp_str.dim = len;
	lisp_str.fillp = len;
	sym = ecl_find_symbol((cl_object)&lisp_str, current_package, &intern);
	if (sym==OBJNULL || sym==Cnil) {
	    RETVAL = &PL_sv_undef;
	} else {
	    RETVAL = create_lisp_sv(aTHX_ PACKAGE_SYMBOL,sym);
	}
    OUTPUT:
    	RETVAL

SV *
s(this, sname)
	SV *this
	SV *sname
    PREINIT:
        int len, intern = 0;
        cl_object sym;
    CODE:
	lisp_str.self = SvPV(sname,len);
	lisp_str.dim = len;
	lisp_str.fillp = len;
	sym = ecl_find_symbol((cl_object)&lisp_str, current_package, &intern);
	if (sym==OBJNULL || sym==Cnil) {
	    RETVAL = &PL_sv_undef;
	} else {
	    RETVAL = create_lisp_sv(aTHX_ PACKAGE_SYMBOL,sym);
	}
    OUTPUT:
    	RETVAL


SV *
create_string(sv)
	SV *sv
    PREINIT:
        int len;
	char *str;
        cl_object x;
    CODE:
        str = SvPV(sv,len);
        x = cl_alloc_simple_base_string(len);
        memcpy(x->base_string.self, str, len);
        /* x->base_string.self[len] = 0; */
	RETVAL = create_lisp_sv(aTHX_ PACKAGE_STRING,x);
    OUTPUT:
        RETVAL

