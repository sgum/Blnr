# Доступ к Exante API

Вкладка «Портфель Exante» показывает текущие позиции счёта. Если доступ к
Exante API не настроен, она автоматически откатывается на портфель,
заданный вручную в `global.R` (`portfolio_holdings`), с котировками с
Yahoo Finance через `quantmod`.

## Получение учётных данных

1. Войдите в личный кабинет Exante (`https://exante.eu`) → раздел
   **API Management**.
2. Создайте приложение (Application) и выберите нужные разрешения
   (scopes): как минимум `accounts`, `summary`, `symbols`, `feed`,
   `crossrates`, `change`, `transactions`.
3. Сохраните три значения: **Client ID**, **Application ID (App ID)** и
   **Shared key**. Shared key показывается один раз — сохраните его сразу.

## Настройка приложения

Скопируйте `.Renviron.example` в `.Renviron` (уже в `.gitignore`, в
репозиторий не попадёт) и заполните:

```
EXANTE_CLIENT_ID=...
EXANTE_APP_ID=...
EXANTE_SHARED_KEY=...
EXANTE_ENV=live   # или demo для тестового контура
```

Перезапустите Shiny-приложение — `R/exante_api.R` подхватит переменные
окружения через `Sys.getenv()`.

## Как это работает

- `R/exante_api.R` — низкоуровневый клиент: собирает подписанный JWT
  (HS256, `iss` = Client ID, `sub` = App ID, подпись — Shared key) и
  дергает REST API v3.0 (`https://api-live.exante.eu` /
  `https://api-demo.exante.eu`).
- `R/portfolio.R` — бизнес-логика: получает список счетов
  (`GET /trade/3.0/accounts`), берёт сводку и позиции первого счёта
  (`GET /trade/3.0/summary/{accountId}/{currency}`) и считает рост
  каждой бумаги от цены закрытия на дату покупки.
- Если `EXANTE_CLIENT_ID` / `EXANTE_APP_ID` / `EXANTE_SHARED_KEY` не
  заданы, или запрос к API возвращает ошибку, приложение молча
  переключается на портфель из `portfolio_holdings` — вкладка никогда не
  падает из-за отсутствия доступа.

## Важно

- **Никогда** не храните `EXANTE_SHARED_KEY` в коде, коммитах или в
  клиентском (браузерном) JavaScript — это боевой ключ доступа к счёту.
- Пути эндпоинтов приведены по документации Exante API v3.0
  (`api-docs.exante.eu`) на момент написания; перед первым боевым
  использованием сверьте их с актуальной документацией — брокерские API
  иногда меняют версии/пути.
- Для многосчётных профилей `get_portfolio_positions()` сейчас берёт
  первый счёт из списка (`accounts[[1]]`) — при необходимости добавьте
  выбор счёта в UI (`selectInput`) и передавайте `account_id` явно.
