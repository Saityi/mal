SRCS = Reader.idr Printer.idr Types.idr Main.idr step0_repl.idr step1_read_print.idr
OTHER_SRCS = Main.idr 
BINS = $(SRCS:%.idr=%)

all: mal

$(BINS): %: %.idr $(OTHER_SRCS)
	idris -p lightyear Main.idr $< -o $@

mal: $(SRCS)
	idris --build Mal.ipkg

clean:
	rm -rf $(BINS)
	idris --clean Mal.ipkg
