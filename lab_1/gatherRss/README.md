# gatherRss

Скрипт написаний мовою Haskell, що виконує наступні функції:
- список фідів береться з текстового файлу
- завантажує фід якщо він доступний (відповідь 200)
- якщо ресурс недоступний то в файл з назвою фіду записується інформація про недоступність ресурсу

To Do:
1. add more friendly interface (--help, --usage, selection of the source file from console)
2. HTTPS support
3. atom support
