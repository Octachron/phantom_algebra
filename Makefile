all: runtest build

build:
	jbuilder build @install

runtest:
	jbuilder runtest --force

clean:
	jbuilder clean

doc:
	jbuilder build @doc
