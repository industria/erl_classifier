AC_INIT([erl_classifier],[0.1.0],[https://github.com/industria/erl_classifier/issues])

AC_REVISION([0.1.0])
AC_COPYRIGHT([Copyright (c) 2011 James Lindstorff. All rights reserved.])

AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL

AC_LANG(Erlang)

AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_CHECK_LIB([stdlib], , [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([kernel], , [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([sasl], , [AC_MSG_ERROR([sasl was not found!])])
AC_ERLANG_CHECK_LIB([mnesia], , [AC_MSG_ERROR([mnesia was not found!])])

AC_CONFIG_SRCDIR(src/ec_classifier.erl)

AC_ARG_VAR([NODENAME], [Name of the Erlang node started (default: ec)])
AS_IF([test -z "$NODENAME"],
	    [AC_SUBST([NODENAME], [ec])]
)


AC_CONFIG_FILES([
	Makefile
	erl_classifier.rel
	src/erl_classifier.app
])

AC_OUTPUT