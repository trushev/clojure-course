## Задача 5.1

Реализуйте параллельный вариант **filter** (не обязательно ленивый) с помощью **future**.
Параллельная обработка должна производиться блоками по заданному числу элементов. Размер
блоков следует вычислять вручную, без использования готовых функций вроде **partition** (для
разделения последовательности следует использовать **take** и **drop**). Продемонстрируйте прирост
производительности в сравнении с обычным фильтром.
