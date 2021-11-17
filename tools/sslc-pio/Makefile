.PHONY: default
default:

SSLC_PIO := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

.PHONY: install
install:
	mkdir -p "$(HOME)/.local/bin" "$(HOME)/.local/share/ssm"
	cp -f "$(SSLC_PIO)/sslc-pio" "$(HOME)/.local/bin"
	rm -rf "$(HOME)/.local/share/ssm/pio-template"
	cp -r "$(SSLC_PIO)/pio-template" "$(HOME)/.local/share/ssm"
