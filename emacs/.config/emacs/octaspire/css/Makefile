.POSIX:

all: base64-style.org

.PHONY: clean

base64-style.org: style.css Makefile
	@rm -f $@
	@printf "#+OPTIONS: html-style:nil\n" >> $@
	@printf "#+HTML_HEAD: <style>\n" >> $@
	@printf "#+HTML_HEAD:   " >> $@
	@cat $< | tr -d '\n' >> $@
	@printf "\n#+HTML_HEAD: </style>\n" >> $@

clean:
	@rm -f base64-style.org
