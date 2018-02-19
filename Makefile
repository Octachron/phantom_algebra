all: runtest
	jbuilder build @install

runtest:
	jbuilder runtest --force
