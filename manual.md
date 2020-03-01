
```sh

docker run --name note -it -d -v /Users/seanliu/Note:/Users/seanliu/Note seanliu/note:1.0.0 /bin/bash

docker exec -it note bash
```