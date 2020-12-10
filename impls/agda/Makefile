SRCS = step0_repl.agda
OTHER_SRCS = main.agda
BINS = $(SRCS:%.agda=%)

all: mal

$(BINS): %: %.agda
	agda --compile main.agda
	mv main $@

# step4_if_fn_do: step4_if_fn_du
# 	mv step4_if_fn_du step4_if_fn_do

mal:
	agda --compile main.agda
	mv main mal

clean:
	rm -rf $(BINS)
	rm -rf *.agdai
#	rm step4_if_fn_do
