TRACEFORMATDIR=$(CURDIR)/../trace-format/
MLDIR=$(CURDIR)/../../libtrace/src/ocaml/piqi

PIQIFILES=$(wildcard *.piqi)
PROTOFILES=$(addsuffix .proto, $(PIQIFILES))
MLFILES=$(addsuffix _piqi.ml, $(basename $(PIQIFILES)))
MLEXTFILES=$(addsuffix _piqi_ext.ml, $(basename $(PIQIFILES)))
PROTOCFILES=$(addsuffix .piqi.pb.cc, $(basename $(PIQIFILES)))
PROTOHFILES=$(addsuffix .piqi.pb.h, $(basename $(PIQIFILES)))

%.piqi.proto : %.piqi
	piqi to-proto $<

%.piqi.pb.cc %.piqi.pb.hh : %.piqi.proto
	protoc $< --cpp_out=.

%_piqi.ml : %.piqi
	piqic ocaml $<

%_piqi_ext.ml : %.piqi
	piqic ocaml-ext $<

all: $(PROTOCFILES) $(PROTOHFILES) $(MLFILES) $(MLEXTFILES)

clean:
	rm -f $(PROTOFILES) $(MLFILES) $(MLEXTFILES) $(PROTOCFILES) $(PROTOHFILES)

.PHONY: install-pb install-piqi install
install-pb: $(PROTOCFILES) $(PROTOHFILES)
	cp $^ $(TRACEFORMATDIR)

install-piqi: $(MLFILES) $(MLEXTFILES)
	cp $^ $(MLDIR)

install: install-pb install-piqi
