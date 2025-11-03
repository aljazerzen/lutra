-- problem: simulate compounding interest with payments

-- At time=0 an amount of money is borrowed.
-- At each time period, the dept is increased by the interest,
--   which is proportional to the amount of current dept.
-- At some time periods (indicated by `payments.period`),
--   the dept is decreased for `payments.amount`.
-- Find the remaining dept at the time of last payment.

-- variables:
--   borrowed amount: 300000.0
--   interest rate per time period: 0.0000005


CREATE TEMPORARY TABLE payments (time INT, amount REAL);

SELECT setseed(0);

WITH RECURSIVE series(i) AS (
  SELECT 1
  UNION ALL
  SELECT i+1 FROM series
   WHERE i+1<=9000
)
INSERT INTO payments (time, amount)
SELECT
    CAST(i * 6 + floor(random() * 3) as INT) AS time,
    round(1 + random() * 9) AS amount
FROM series;



-- solution 0: iterate over time
-- Planning Time: 0.212 ms
-- Execution Time: 29376.000 ms
EXPLAIN (ANALYZE, VERBOSE, TIMING, SUMMARY, MEMORY)
WITH RECURSIVE current AS (
  SELECT -- initial state
    0 AS time,
    300000.0::real AS dept

  UNION ALL

  SELECT -- iterate over time
    current.time + 1,
    GREATEST(0.0, (
        current.dept * (1.0 + 0.0000005) - COALESCE(p.amount, 0)
    )::real) AS current
  FROM current
  LEFT JOIN payments p ON p.time = current.time + 1 -- indexed lookup
  WHERE current.dept > 0.0 AND current.time + 1 <= (SELECT MAX(time) FROM payments)
)
SELECT * FROM current
ORDER BY time DESC LIMIT 1;

-- solution 1: iterate over time, with index
-- Planning Time: 0.130 ms
-- Execution Time: 76.491 ms
CREATE TEMPORARY TABLE payments_1 (time int, amount real);
INSERT INTO payments_1 SELECT time, amount FROM payments;
CREATE INDEX payments_1_idx_time ON payments_1 USING BTREE (time);

EXPLAIN (ANALYZE, VERBOSE, TIMING, SUMMARY, MEMORY)
WITH RECURSIVE current AS (
  SELECT -- initial state
    0 AS time,
    300000.0::real AS dept

  UNION ALL

  SELECT -- iterate over time
    current.time + 1,
    GREATEST(0.0, (
        current.dept * (1.0 + 0.0000005) - COALESCE(p.amount, 0)
    )::real) AS current
  FROM current
  LEFT JOIN payments_1 p ON p.time = current.time + 1 -- indexed lookup
  WHERE current.dept > 0.0 AND current.time + 1 <= (SELECT MAX(time) FROM payments_1)
)
SELECT * FROM current
ORDER BY time DESC LIMIT 1;


-- solution 2: iterate over payments
-- Planning Time: 0.086 ms
-- Execution Time: 1875.198 ms
EXPLAIN (ANALYZE, VERBOSE, TIMING, SUMMARY, MEMORY)

WITH RECURSIVE dept(index, time, amount) AS (
  SELECT
    0 AS index,
    0 AS time,
    300000.0::real AS amount

  UNION ALL

  SELECT
    dept.index + 1,
    payment.time,
    GREATEST(0.0, (
      dept.amount
      * pow(1.0 + 0.0000005, payment.time - dept.time)
      - payment.amount
    )::real) AS dept
  FROM
    dept,
    lateral (SELECT * FROM payments OFFSET dept.index LIMIT 1) payment
  WHERE dept.amount > 0.0
)
SELECT * FROM dept
ORDER BY index DESC LIMIT 1;



-- solution 3: iterate over payments, with index
-- Planning Time: 0.111 ms
-- Execution Time: 17.799 m
CREATE TEMPORARY TABLE payments_2 (index int, time int, amount real);
INSERT INTO payments_2 SELECT ROW_NUMBER() OVER (), time, amount FROM payments;
CREATE INDEX payments_2_idx_index ON payments_2 USING BTREE (index);

EXPLAIN (ANALYZE, VERBOSE, TIMING, SUMMARY, MEMORY)
WITH RECURSIVE dept(index, time, amount) AS (
  SELECT
    0 AS index,
    0 AS time,
    300000.0::real AS amount

  UNION ALL

  SELECT
    dept.index + 1,
    payment.time,
    GREATEST(0.0, (
      dept.amount
      * pow(1.0 + 0.0000005, payment.time - dept.time)
      - payment.amount
    )::real) AS dept
  FROM
    dept,
    lateral (SELECT * FROM payments_2 p WHERE p.index = dept.index + 1) payment
  WHERE dept.amount > 0.0
)
SELECT * FROM dept
ORDER BY index DESC LIMIT 1;



-- solution 4: aggregate payments
-- Planning Time: 0.042 ms
-- Execution Time: 8.309 ms

CREATE TYPE dept AS (
    time int,
    amount real
);

CREATE OR REPLACE FUNCTION reduce_payment(
    current dept,
    payment payments
)
RETURNS dept
AS $$
BEGIN RETURN ROW(
    payment.time,
    GREATEST(0.0, (
        current.amount
        * pow(1.0 + 0.0000005, payment.time - current.time)
        - payment.amount
    )::REAL)
)::dept; END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE AGGREGATE aggregate_payments(payments)
(
    sfunc = reduce_payment,
    stype = dept,
    initcond = '(0,300000.0)'
);

EXPLAIN (ANALYZE, VERBOSE, TIMING, SUMMARY, MEMORY)
SELECT aggregate_payments(payments.*) FROM payments;
