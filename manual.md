
```sh
# build docker image
docker build -t seanliu/note:1.0.0 .

docker run --name note -it -d  -p 9000:9000 -v /Users/seanliu/Note:/Users/seanliu/Note seanliu/note:1.0.0 /bin/bash

docker exec -it note bash
```