NAME:=tjr_pcache_docker_image

all:
	$(MAKE) from_github
	$(MAKE) test

from_github:
	docker build -t $(NAME)_from_github -f Dockerfile.from_github .
#	docker run -ti $NAME bash

test:
	docker build -t $(NAME)_test -f Dockerfile.test .

clean:
	-docker image rm $(NAME)_from_github $(NAME)_test
