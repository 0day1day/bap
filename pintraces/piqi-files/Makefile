ROOTDIR=$(CURDIR)/../../
TRACEFORMATDIR=$(CURDIR)/../trace-format/
MLDIR=$(ROOTDIR)/libtrace/src/ocaml/piqi

PIQIFILES=$(wildcard *.piqi)
PROTOFILES=$(addsuffix .proto, $(PIQIFILES))
MLFILES=$(addsuffix _piqi.ml, $(basename $(PIQIFILES)))
MLEXTFILES=$(addsuffix _piqi_ext.ml, $(basename $(PIQIFILES)))
PROTOCFILES=$(addsuffix .piqi.pb.cc, $(basename $(PIQIFILES)))
PROTOHFILES=$(addsuffix .piqi.pb.h, $(basename $(PIQIFILES)))

PIQIDIR=$(ROOTDIR)/piqi/piqi/bin
PIQI=$(PIQIDIR)/piqi
PIQIC=$(PIQIDIR)/piqic
PROTOC=$(ROOTDIR)/protobuf/src/protoc

PIQIFLAGS = --pp
#PIQIFLAGS = --pp --leave-tmp-files

%.piqi.proto : %.piqi
	$(PIQI) to-proto $<

%.piqi.pb.cc %.piqi.pb.hh : %.piqi.proto
	$(PROTOC) $< --cpp_out=.

#%_piqi.ml : %.piqi
#	piqic ocaml $(PIQIFLAGS) $<

%_piqi.ml %_piqi_ext.ml : %.piqi
	$(PIQIC) ocaml-ext $(PIQIFLAGS) $<

all: $(PROTOCFILES) $(PROTOHFILES) $(MLFILES) $(MLEXTFILES)

clean:
	rm -f $(PROTOFILES) $(MLFILES) $(MLEXTFILES) $(PROTOCFILES) $(PROTOHFILES)

.PHONY: install-pb install-piqi install
install-pb: $(PROTOCFILES) $(PROTOHFILES)
	cp $^ $(TRACEFORMATDIR)

install-piqi: $(MLFILES) $(MLEXTFILES)
	cp $^ $(MLDIR)

install: install-pb install-piqi
