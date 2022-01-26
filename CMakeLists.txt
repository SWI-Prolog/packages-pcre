cmake_minimum_required(VERSION 3.5)
project(swipl-pcre)

include("../cmake/PrologPackage.cmake")

find_package(PCRE)
if(PCRE_FOUND)

  swipl_plugin(
    pcre
    MODULE pcre4pl
    C_SOURCES pcre4pl.c
    C_LIBS ${PCRE_LIBRARIES}
    PL_LIBS pcre.pl)
  target_include_directories(
    plugin_pcre4pl BEFORE PRIVATE
    ${PCRE_INCLUDE_DIR})

  install_dll(${PCRE_LIBRARIES})

  test_libs(pcre)

  pkg_doc(pcre
	SECTION
	SOURCE pcre.pl pcre4pl.tex)

endif(PCRE_FOUND)
