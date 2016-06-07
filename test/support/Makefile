JAVA_VERSION ?= 1.8

all :
	javac -g -target $(JAVA_VERSION) -source $(JAVA_VERSION) -cp . *.java

%.class : %.java
	javac -g -target $(JAVA_VERSION) -source $(JAVA_VERSION) -cp . $<

JAVA7_FILES := Arrays Errors IVTDriver IVTSub IVTSuper TestMethodSpec Trivial
.PHONY: java7
java7:
	$(foreach file,$(JAVA7_FILES),$(MAKE) JAVA_VERSION=1.7 $(file).class;)

.PHONY: clean
clean:
	rm *.class
