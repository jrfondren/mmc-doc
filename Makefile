BIN=mmc-doc
DEPS=config.m libinfo.m mmcdoc.m popen.m html.m
FILES=$(patsubst %.m,%,$(DEPS))
GENEXT=d,o,mh,err,c,c_date,mh
GRADE=hlc.gc
FLAGS=-s $(GRADE) -O4 --intermodule-optimization

all:: $(BIN)

install:: $(BIN)
	mv -f -v $(BIN) $(HOME)/bin/

%: %.m $(DEPS)
	mmc $(FLAGS) --make $@

$(BIN): $(DEPS)
	mmc $(FLAGS) --make mmcdoc
	mv -fv mmcdoc $(BIN)

clean::
	rm -rf Mercury
	rm -fv $$(for x in $(FILES); do echo $$x.{$(GENEXT)}; done)
	rm -fv $(BIN)
