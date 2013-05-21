drop table if exists asia;
create table asia (alpha char(1) not null, p real);

insert into asia(alpha, p) values ('y', 0.01);
insert into asia(alpha, p) values ('n', 0.99);


drop table if exists tuberculosis;
create table tuberculosis(tau char(1), alpha char(1), p real);

insert into tuberculosis(tau, alpha, p) values ('y', 'y', 0.05);
insert into tuberculosis(tau, alpha, p) values ('y', 'n', 0.01);
insert into tuberculosis(tau, alpha, p) values ('n', 'y', 0.95);
insert into tuberculosis(tau, alpha, p) values ('n', 'n', 0.99);


drop table if exists smoking;
create table smoking (sigma char(1) not null, p real);

insert into smoking(sigma, p) values ('y', 0.5);
insert into smoking(sigma, p) values ('n', 0.5);


drop table if exists lungcancer;
create table lungcancer(lambda char(1), sigma char(1), p real);

insert into lungcancer(lambda, sigma, p) values ('y', 'y', 0.1);
insert into lungcancer(lambda, sigma, p) values ('y', 'n', 0.01);
insert into lungcancer(lambda, sigma, p) values ('n', 'y', 0.9);
insert into lungcancer(lambda, sigma, p) values ('n', 'n', 0.99);


drop table if exists bronchitis;
create table bronchitis(beta char(1), sigma char(1), p real);

insert into bronchitis(beta, sigma, p) values ('y', 'y', 0.6);
insert into bronchitis(beta, sigma, p) values ('y', 'n', 0.3);
insert into bronchitis(beta, sigma, p) values ('n', 'y', 0.4);
insert into bronchitis(beta, sigma, p) values ('n', 'n', 0.7);


drop table if exists tublungcancer;
create table tublungcancer(epsilon char(1), lambda char(1), tau char(1), p real);

insert into tublungcancer(epsilon, lambda, tau, p) values ('y', 'y', 'y', 1);
insert into tublungcancer(epsilon, lambda, tau, p) values ('y', 'y', 'n', 1);
insert into tublungcancer(epsilon, lambda, tau, p) values ('y', 'n', 'y', 1);
insert into tublungcancer(epsilon, lambda, tau, p) values ('y', 'n', 'n', 0);
insert into tublungcancer(epsilon, lambda, tau, p) values ('n', 'y', 'y', 0);
insert into tublungcancer(epsilon, lambda, tau, p) values ('n', 'y', 'n', 0);
insert into tublungcancer(epsilon, lambda, tau, p) values ('n', 'n', 'y', 0);
insert into tublungcancer(epsilon, lambda, tau, p) values ('n', 'n', 'n', 1);


drop table if exists xray;
create table xray(psy char(1), epsilon char(1), p real);

insert into xray(psy, epsilon, p) values ('y', 'y', 0.98);
insert into xray(psy, epsilon, p) values ('y', 'n', 0.05);
insert into xray(psy, epsilon, p) values ('n', 'y', 0.02);
insert into xray(psy, epsilon, p) values ('n', 'n', 0.95);


drop table if exists dyspnoea;
create table dyspnoea(delta char(1), epsilon char(1), beta char(1), p real);

insert into dyspnoea(delta, epsilon, beta, p) values ('y', 'y', 'y', 0.9);
insert into dyspnoea(delta, epsilon, beta, p) values ('y', 'y', 'n', 0.7);
insert into dyspnoea(delta, epsilon, beta, p) values ('y', 'n', 'y', 0.8);
insert into dyspnoea(delta, epsilon, beta, p) values ('y', 'n', 'n', 0.1);
insert into dyspnoea(delta, epsilon, beta, p) values ('n', 'y', 'y', 0.1);
insert into dyspnoea(delta, epsilon, beta, p) values ('n', 'y', 'n', 0.3);
insert into dyspnoea(delta, epsilon, beta, p) values ('n', 'n', 'y', 0.3);
insert into dyspnoea(delta, epsilon, beta, p) values ('n', 'n', 'n', 0.9);