all: modules.svg

lib.list : 
	saga_cmd | grep "^ *-" | sed 's/^ *- //' > $@

module.list :  lib.list
	rm -f $@
	for l in $$(cat $<); do \
		echo $$l >> $@ ;\
		echo "--------------------------------" >> $@ ;\
		saga_cmd $$l | grep -E "^ \[[0-9]+" \
		| awk -v l=$$l '{print l, ":",$$0}' \
			>> $@ ;\
		echo "\n" >> $@ ;\
	done

modules.dot : module.list
	echo "digraph modules {\n\trankdir = LR;" > $@
	awk -F ":" 'NF == 3 {printf "\t\"%s\" -> \"%s\";\n", $$1, $$3}' \
		< $< >> $@ 
	echo "}" >> $@

modules.svg : modules.dot
	dot  -Tsvg $< -o $@

.PHONY: clean

clean:
	rm -f module.list lib.list modules.dot modules.png
