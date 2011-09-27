all:	stp/bin/stp

stp/bin/stp:
ifeq (,$(findstring stp/bin,${PATH}))
	cd ./stp && ./scripts/configure && make 
else 
	@echo "stp/bin is already in path; Not making STP." 
endif

clean:
	echo Not cleaning STP
#	-cd ./stp && make clean

tags: 

.PHONY: tags
