
```sh
# build docker image
docker build -t seanliu/note:1.0.0 .

docker run --rm -it -p 9000:9000 -v /Users/seanliu/Note:/Users/seanliu/Note -d seanliu/note:1.0.0

docker exec -it note bash
```