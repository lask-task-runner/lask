FROM golang:1.24.3

WORKDIR /app

COPY go.mod go.mod
COPY go.sum go.sum
COPY main.go main.go
COPY main_test.go main_test.go

RUN go build -o ./main .

CMD [ "./main" ]
