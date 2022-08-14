SRC=src/blink1

SRC_SSL=$(SRC).ssl
SRC_C=$(SRC).c
SRC_C_PREBUILT=$(SRC).c.backup

SSLC=sslc

PIO=pio
PIO_DIR=.pio
DEPS=src include platform

LIBSSM_TGT=$(PIO_DIR)/libdeps/nrf52840_dk/ssm

.PHONY: build clean_lib clean distclean
build: clean_lib $(SRC_C)
	$(PIO) run

clean_lib:
	rm -rf $(LIBSSM_TGT)

clean:
	$(PIO) run -t clean

distclean:
	rm -rf $(PIO_DIR)

ifeq ($(shell command -v $(SSLC)),)
$(info # SSLC not available, using pre-compiled C source)
$(SRC_C): $(SRC_C_PREBUILT)
	cp $(SRC_C) $(SRC_C_PREBUILT)
else
$(SRC_C): $(SRC_SSL)
	$(SSLC) $< > $@
endif
