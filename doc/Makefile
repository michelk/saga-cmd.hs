
lib.list : 
	saga_cmd | grep "\- lib" | sed -e 's/\- //' -e 's/\.so//' > $@

module.list : 
	rm -f $@
	for l in $$(cat lib.list); do \
		echo $$l >> $@ ;\
		echo "--------------------------------" >> $@ ;\
		saga_cmd $$l | grep "\- " \
		| awk -F '- ' -v l=$$l 'BEGIN{OFS=":";} {print l, $$1,$$2}' \
			>> $@ ;\
		echo "\n" >> $@ ;\
	done

modules.dot : module.list
	echo "digraph modules {\n\trankdir = LR;" > $@
	awk -F ":" 'NF == 3 {printf "\t\"%s\" -> \"%s\";\n", $$1, $$3}' \
		< $< >> $@ 
	echo "}" >> $@

modules.png : modules.dot
	dot  -Tpng $< -o $@

