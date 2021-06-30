# REST JSON simple server

## Short description

The server operates in the REST architecture mode. The server can accept POST requests and respond with JSON objects. The server simulates a simple online store capable of creating customers, products, and adding products to a customer cart.

## Server testing

> curl -k -H "Content-Type: application/json" -X POST "https://localhost:8080/addClient.json?id=1&name=vasya"

```
{"result":"OK"}
```
> curl -k -d '{"shrtName":"item1", "fullName": "item num 1", "descr":"description for item1", "cost":500}' -H "Content-Type: application/json" -X POST "https://localhost:8080/addItem.json"

```
{"result":"OK"}
```
> curl -k -d '{"shrtName":"item2", "fullName": "item num 2", "descr":"description for item2", "cost":1500}' -H "Content-Type: application/json" -X POST "https://localhost:8080/addItem.json"

```
{"result":"OK"}
```
> curl -k -H "Content-Type: application/json" -X POST "https://localhost:8080/getItem.json?iId=1"

```
{"shrtName":"item1","fullName":"item num 1","descr":"description for item1","cost":500,"result":"OK"}
```
> curl -k -H "Content-Type: application/json" -X POST "https://localhost:8080/getClientByName.json?name=vasya"

```
{"cId":1,"result":"OK"}
```
> curl -k -H "Content-Type: application/json" -X POST "https://localhost:8080/addToBasket.json?cId=1&iId=2&cost=500"

```
{"result":"OK"}
```
> curl -k -H "Content-Type: application/json" -X POST "https://localhost:8080/addToBasket.json?cId=1&iId=1&cost=500"

```
{"result":"OK"}
```
> curl -k -H "Content-Type: application/json" -X POST "https://localhost:8080/addToBasket.json?cId=1&iId=1&cost=500"

```
{"result":"OK"}
```
> curl -k -H "Content-Type: application/json" -X POST "https://localhost:8080/getBasket.json?cId=1"

```
[{"iId":2,"cost":500},{"iId":1,"cost":500}]
```
