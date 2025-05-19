FROM golang:1.24.3-alpine3.21 AS builder

WORKDIR /app

COPY go.mod go.mod
COPY go.sum go.sum
COPY main.go main.go
COPY main_test.go main_test.go

RUN go build -o ./main .

FROM alpine:3.21
WORKDIR /app
COPY --from=builder /app/main .

CMD [ "./main" ]
