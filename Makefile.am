if ENABLE_OPENMP
AM_FCFLAGS += $(OPENMP_FCLAGS)
AM_LDFLAGS += $(OPENMP_FCLAGS)
endif

if ENABLE_OPENACC
if NVIDIA
AM_FCFLAGS += -acc=gpu,host -cuda -Minfo=accel -Minline
endif
if GCC
AM_FCFLAGS += -fopenacc
endif
endif

SUBDIRS=src
