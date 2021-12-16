# -*- coding: utf-8 -*-
#===============================================================================
## \file setup.py
# \brief
# \b DUMSES-Hybrid:
# This is dumpy install script
# \copyright
# Copyrights 2013-2021, CEA.
# This file is distributed under the CeCILL-A & GNU/GPL licenses, see
# <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
# <http://www.gnu.org/licenses/>

#===============================================================================
import numpy
from numpy.distutils.core import setup, Extension

PATH_INCLUDES = [numpy.get_include()]
PATH_LIBRARIES = ['build']
LINK_LIBRARIES = []

setup(
    name = "dumpy",
    version = "1.0",
    packages = ["dumpy", "dumpy.data", "dumpy.utils", "dumpy.plots" \
                       , "dumpy.analysis"],
    author = "Marc Joos, Sébastien Fromang",
    author_email = "marc.joos@cea.fr",
    license = "CECILL-2.1, GPL-3.0.-only",
    )
