## Задача 7.1

Задан набор аэропортов, авиамаршрутов (каждый характеризуется начальным и конечным аэропортом), количество билетов на каждый маршрут и их цены. На основе этих данных реализуйте программу, имитирующую одновременное бронирование авиабилетов.

В общем случае для перелета между двумя точками могут потребоваться пересадки. При этом клиента интересует бронирование на все промежуточные перелеты одновременно, либо нотификация, что это невозможно по причине отсутствия свободных билетов. При бронировании следует отдавать предпочтение той последовательности перелетов, которая дешевле (даже если количество перелетов при этом больше).

Дополните предоставленный код, используйте STM для атомарности бронирования. Настройте задержки таким образом, чтобы все клиенты (представленные потоками исполнения) смогли забронировать билеты.