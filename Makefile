golok: lookup-table.scm datatypes.scm model-builder.scm search.scm golok.scm find-k.scm parser.scm
	mzc --exe golok golok.scm

render:
	output/render.sh

depclean:
	rm -rf examples/*.dot
	rm -rf test/*.dot
	rm -rf *.dot
	rm -f golok

clean:
	rm -rf output/*.dot
	rm -rf examples/*.dot
	rm -rf output/*.topo
	rm -rf output/*.svg
