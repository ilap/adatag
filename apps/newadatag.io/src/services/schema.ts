// FIXME: from which slot it should start query.
const BOOTSTRAP_SLOT = 10

// DB Worker constants
export const DBNAME = `adatagdb.sqlite3`
export const SCHEMA = `
CREATE TABLE IF NOT EXISTS config ( id INTEGER PRIMARY KEY, tip INTEGER NOT NULL );
INSERT OR IGNORE INTO config (id, tip) VALUES (1, ${BOOTSTRAP_SLOT});

-- IntegriTree tables 
CREATE TABLE IF NOT EXISTS a ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS b ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS c ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS d ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS e ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS f ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS g ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS h ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS i ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS j ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );

CREATE TABLE IF NOT EXISTS k ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS l ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS m ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS n ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS o ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS p ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS q ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS r ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS s ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS t ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );

CREATE TABLE IF NOT EXISTS u ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS v ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS w ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS x ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS y ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );
CREATE TABLE IF NOT EXISTS z ( xi INTEGER PRIMARY KEY, xa TEXT NOT NULL, xb TEXT NOT NULL );

CREATE INDEX IF NOT EXISTS idx_a_xa ON a (xa);
CREATE INDEX IF NOT EXISTS idx_a_xb ON a (xb);

CREATE INDEX IF NOT EXISTS idx_b_xa ON b (xa);
CREATE INDEX IF NOT EXISTS idx_b_xb ON b (xb);

CREATE INDEX IF NOT EXISTS idx_c_xa ON c (xa);
CREATE INDEX IF NOT EXISTS idx_c_xb ON c (xb);

CREATE INDEX IF NOT EXISTS idx_d_xa ON d (xa);
CREATE INDEX IF NOT EXISTS idx_d_xb ON d (xb);

CREATE INDEX IF NOT EXISTS idx_e_xa ON e (xa);
CREATE INDEX IF NOT EXISTS idx_e_xb ON e (xb);

CREATE INDEX IF NOT EXISTS idx_f_xa ON f (xa);
CREATE INDEX IF NOT EXISTS idx_f_xb ON f (xb);

CREATE INDEX IF NOT EXISTS idx_g_xa ON g (xa);
CREATE INDEX IF NOT EXISTS idx_g_xb ON g (xb);

CREATE INDEX IF NOT EXISTS idx_h_xa ON h (xa);
CREATE INDEX IF NOT EXISTS idx_h_xb ON h (xb);

CREATE INDEX IF NOT EXISTS idx_i_xa ON i (xa);
CREATE INDEX IF NOT EXISTS idx_i_xb ON i (xb);

CREATE INDEX IF NOT EXISTS idx_j_xa ON j (xa);
CREATE INDEX IF NOT EXISTS idx_j_xb ON j (xb);

CREATE INDEX IF NOT EXISTS idx_k_xa ON k (xa);
CREATE INDEX IF NOT EXISTS idx_k_xb ON k (xb);

CREATE INDEX IF NOT EXISTS idx_l_xa ON l (xa);
CREATE INDEX IF NOT EXISTS idx_l_xb ON l (xb);

CREATE INDEX IF NOT EXISTS idx_m_xa ON m (xa);
CREATE INDEX IF NOT EXISTS idx_m_xb ON m (xb);

CREATE INDEX IF NOT EXISTS idx_n_xa ON n (xa);
CREATE INDEX IF NOT EXISTS idx_n_xb ON n (xb);

CREATE INDEX IF NOT EXISTS idx_o_xa ON o (xa);
CREATE INDEX IF NOT EXISTS idx_o_xb ON o (xb);

CREATE INDEX IF NOT EXISTS idx_p_xa ON p (xa);
CREATE INDEX IF NOT EXISTS idx_p_xb ON p (xb);

CREATE INDEX IF NOT EXISTS idx_q_xa ON q (xa);
CREATE INDEX IF NOT EXISTS idx_q_xb ON q (xb);

CREATE INDEX IF NOT EXISTS idx_r_xa ON r (xa);
CREATE INDEX IF NOT EXISTS idx_r_xb ON r (xb);

CREATE INDEX IF NOT EXISTS idx_s_xa ON s (xa);
CREATE INDEX IF NOT EXISTS idx_s_xb ON s (xb);

CREATE INDEX IF NOT EXISTS idx_t_xa ON t (xa);
CREATE INDEX IF NOT EXISTS idx_t_xb ON t (xb);

CREATE INDEX IF NOT EXISTS idx_u_xa ON v (xa);
CREATE INDEX IF NOT EXISTS idx_u_xb ON v (xb);

CREATE INDEX IF NOT EXISTS idx_v_xa ON u (xa);
CREATE INDEX IF NOT EXISTS idx_v_xb ON u (xb);

CREATE INDEX IF NOT EXISTS idx_w_xa ON w (xa);
CREATE INDEX IF NOT EXISTS idx_w_xb ON w (xb);

CREATE INDEX IF NOT EXISTS idx_x_xa ON x (xa);
CREATE INDEX IF NOT EXISTS idx_x_xb ON x (xb);

CREATE INDEX IF NOT EXISTS idx_y_xa ON y (xa);
CREATE INDEX IF NOT EXISTS idx_y_xb ON y (xb);

CREATE INDEX IF NOT EXISTS idx_z_xa ON z (xa);
CREATE INDEX IF NOT EXISTS idx_z_xb ON z (xb);


INSERT INTO a (xi, xa, xb) VALUES ( '0', '\`',  'b');
INSERT INTO b (xi, xa, xb) VALUES ( '0', 'a',  'c');
INSERT INTO c (xi, xa, xb) VALUES ( '0', 'b',  'd');
INSERT INTO d (xi, xa, xb) VALUES ( '0', 'c',  'e');
INSERT INTO e (xi, xa, xb) VALUES ( '0', 'd',  'f');
INSERT INTO f (xi, xa, xb) VALUES ( '0', 'e',  'g');
INSERT INTO g (xi, xa, xb) VALUES ( '0', 'f',  'h');
INSERT INTO h (xi, xa, xb) VALUES ( '0', 'g',  'i');
INSERT INTO i (xi, xa, xb) VALUES ( '0', 'h',  'j');
INSERT INTO j (xi, xa, xb) VALUES ( '0', 'i',  'k');

INSERT INTO k (xi, xa, xb) VALUES ( '0', 'j',  'l');
INSERT INTO l (xi, xa, xb) VALUES ( '0', 'k',  'm');
INSERT INTO m (xi, xa, xb) VALUES ( '0', 'l',  'n');
INSERT INTO n (xi, xa, xb) VALUES ( '0', 'm',  'o');
INSERT INTO o (xi, xa, xb) VALUES ( '0', 'n',  'p');
INSERT INTO p (xi, xa, xb) VALUES ( '0', 'o',  'q');
INSERT INTO q (xi, xa, xb) VALUES ( '0', 'p',  'r');
INSERT INTO r (xi, xa, xb) VALUES ( '0', 'q',  's');
INSERT INTO s (xi, xa, xb) VALUES ( '0', 'r',  't');
INSERT INTO t (xi, xa, xb) VALUES ( '0', 's',  'u');

INSERT INTO u (xi, xa, xb) VALUES ( '0', 't',  'v');
INSERT INTO v (xi, xa, xb) VALUES ( '0', 'u',  'w');
INSERT INTO w (xi, xa, xb) VALUES ( '0', 'v',  'x');
INSERT INTO x (xi, xa, xb) VALUES ( '0', 'w',  'y');
INSERT INTO y (xi, xa, xb) VALUES ( '0', 'x',  'z');
INSERT INTO z (xi, xa, xb) VALUES ( '0', 'y',  '{');
`

// EXPLAIN QUERY PLAN SELECT * FROM sampleVals2 WHERE xa < 'ubul' AND 'ubul' < xb;
export const nonmemberQuery = (tableName: string, element: string) =>
  `SELECT * FROM ${tableName} WHERE xa < ${element} AND ${element} < xb;`

export const memberQuery = (element: string) =>
  `SELECT * FROM {tableName}  WHERE xa = ${element} OR ${element} = xb;`

export const appendUpdateQuery = (tableName: string, element: string, tip: number) => `
BEGIN TRANSACTION;

  -- Insert a new row only if xa < 'x' AND 'x' < xb holds.
  INSERT OR ROLLBACK INTO ${tableName} (xi, xa, xb) VALUES (
	  (SELECT MAX(xi) + 1 FROM ${tableName}), 
	  '${element}', 
	  (SELECT xb FROM ${tableName} where xa < '${element}' AND '${element}' < xb)
  );

  UPDATE config SET tip = '${tip}' WHERE id = 1;

  -- Update the row with the new value of xb where xa < 'x' and 'x' < xb
  UPDATE ${tableName} SET xb = '${element}' WHERE xa < '${element}' AND '${element}' < xb;

-- COMMIT;
END TRANSACTION;
`
export const slotQuery = () => `SELECT MAX(tip) AS last_slot FROM config;`
