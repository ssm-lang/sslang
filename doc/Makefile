TEX = pdflatex --interaction=nonstopmode

TARGET ?= sslang-lrm

$(TARGET).pdf: $(TARGET).tex
	$(TEX) $<

ifndef OPEN
ifeq ($(shell which open 1>/dev/null 2>/dev/null; echo $$?), 0)
OPEN:=open
else ifeq ($(shell which mimeo 1>/dev/null 2>/dev/null; echo $$?), 0)
OPEN:=mimeo
else ifeq ($(shell which xdg-open 1>/dev/null 2>/dev/null; echo $$?), 0)
OPEN:=xdg-open
endif
endif

ifdef OPEN
open o: $(TARGET).pdf
	$(OPEN) $(TARGET).pdf
endif

PHONY += clean
clean:
	rm -rf *.log data/*.pdf data/*.eps \
        *.aux $(TARGET).pdf *.bbl *.blg *.toc \
	*-tb.exe *.v.exe *.v *.vcd $(TARGET).out *.nav \
	*.snm *.vrb *.gold *.tbout *.tmp *.diff *.dot

.PHONY: $(PHONY)
