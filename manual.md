
```sh
# build docker image
docker build -t seanliu/note:1.0.0 .

docker run --name note -it -p 9090:9000 -v /Users/seanliu/Note:/home/note -d seanliu/note:1.0.0

docker exec -it note bash
```