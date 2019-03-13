SRCS = step0_repl.idr step1_read_print.idr step2_eval.idr step3_env.idr \
       step4_if_fn_do.idr step5_tco.idr step6_file.idr step7_quote.idr \
       step8_macros.idr step9_try.idr stepA_mal.idr
OTHER_SRCS = Main.idr 
BINS = $(SRCS:%.idr=%)

all: mal

$(BINS): %: %.idr $(OTHER_SRCS)
	idris -i ../../lightyear Main.idr $< -o $@

mal: 
	idris --build Mal.ipkg

clean:
	rm -rf $(BINS)
	idris --clean Mal.ipkg
