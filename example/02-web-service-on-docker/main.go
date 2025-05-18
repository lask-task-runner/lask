package main

import (
	"net/http"

	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"
)

func main() {
	e := NewRouter()
	e.Logger.Fatal(e.Start(":8000"))
}

func NewRouter() *echo.Echo {
	e := echo.New()

	e.Use(middleware.Logger())
	e.Use(middleware.Recover())

	e.GET("/", hello)

	return e
}

func hello(c echo.Context) error {
	return c.String(http.StatusOK, "Hello, Lask!")
}
