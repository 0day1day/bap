all:	stp/bin/stp

stp/bin/stp:
ifeq (,$(shell which stp))
	cd ./stp && ./scripts/configure && $(MAKE)
else 
	@echo "stp is already in path; Not making STP." 
endif

clean:
	echo Not cleaning STP
#	-cd ./stp && make clean

tags: 

.PHONY: tags
