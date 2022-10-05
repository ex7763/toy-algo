all: build
build: 
	ros run -l 'glfw3-quadtree.asd' \
		--eval '(ql:quickload :glfw3-quadtree)' \
		--eval '(asdf:make :glfw3-quadtree)' \
		--eval '(quit)'

clean:
	rm -f quadtree quadtree.exe
