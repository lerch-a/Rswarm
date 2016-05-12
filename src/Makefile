
all: swarm/swarm ../inst/swarm

swarm/swarm:
	cd swarm/src; \
	make -f Makefile; \
	cd ../..; \
	echo “Make of Swarm DONE”

../inst/swarm: swarm/swarm
	mv swarm/bin ../inst
	echo “Copy of Swarm DONE”

.PHONY: clean
clean:
	rm -f swarm/*.o
	rm -f ../inst/swarm ../inst/
