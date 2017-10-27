CREATE TABLE `Users` (
  `u_id`      INT UNSIGNED AUTO_INCREMENT  NOT NULL,
  `u_email`   VARCHAR(254)                 NOT NULL,
  `u_name`    VARCHAR(35)                  NOT NULL,
  `u_surname` VARCHAR(35)                  NOT NULL,
  `u_passw`   CHAR(60)                     NOT NULL,
  PRIMARY KEY (`u_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
