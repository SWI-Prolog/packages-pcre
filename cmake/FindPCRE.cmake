# Copyright (C) 2007-2009 LuaDist.
# Created by Peter Kapec <kapecp@gmail.com>
# Redistribution and use of this file is allowed according to the terms of the MIT license.
# For details see the COPYRIGHT file distributed with LuaDist.
#	Note:
#		Searching headers and libraries is very simple and is NOT as powerful as scripts
#		distributed with CMake, because LuaDist defines directories to search for.
#		Everyone is encouraged to contact the author with improvements. Maybe this file
#		becomes part of CMake distribution sometimes.

# - Find pcre
# Find the native PCRE headers and libraries.
#
#   NOTE: PCRE_FOUND refers to library(pcre) [Prolog package]
#         PCRE_LIBRARIES, PCRE_INCLUDE_DIRS refer to the foreign code (libpcre2-8.so etc)
#
# PCRE_INCLUDE_DIRS	- where to find pcre2.h, etc.
# PCRE_LIBRARIES	- List of libraries when using pcre2.
# PCRE_FOUND		- True if pcre2 found.

# Look for the header file.
# TODO: verify the version is at least 10.39. (oldest pcre2 version is 10.10: https://www.pcre.org/changelog.txt)
find_path(PCRE_INCLUDE_DIR NAMES pcre2.h)

# Look for the library
# - sets PCRE_LIBRARY (e.g.: /usr/lib/x86_64-linux-gnu/libpcre2-8.so),
#        PCRE_INCLUDE_DIR (e.g., /usr/include)
find_library(PCRE_LIBRARY NAMES pcre2-8)

# Handle the QUIETLY and REQUIRED arguments and set PCRE_FOUND to TRUE if all listed variables are TRUE.
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(PCRE DEFAULT_MSG PCRE_LIBRARY PCRE_INCLUDE_DIR)

# Copy the results to the output variables.
if(PCRE_FOUND)
	set(PCRE_LIBRARIES ${PCRE_LIBRARY})
	set(PCRE_INCLUDE_DIRS ${PCRE_INCLUDE_DIR})
else(PCRE_FOUND)
	set(PCRE_LIBRARIES)
	set(PCRE_INCLUDE_DIRS)
endif(PCRE_FOUND)

mark_as_advanced(PCRE_INCLUDE_DIR PCRE_LIBRARY PCRE_INCLUDE_DIRS PCRE_LIBRARIES)
