drop table if exists asia;
create table asia (a char(1) not null, p real);

insert into asia(a, p) values ('y', 0.01);
insert into asia(a, p) values ('n', 0.99);


drop table if exists tuberculosis;
create table tuberculosis(t char(1), a char(1), p real);

insert into tuberculosis(t, a, p) values ('y', 'y', 0.05);
insert into tuberculosis(t, a, p) values ('y', 'n', 0.01);
insert into tuberculosis(t, a, p) values ('n', 'y', 0.95);
insert into tuberculosis(t, a, p) values ('n', 'n', 0.99);


drop table if exists smoking;
create table smoking (s char(1) not null, p real);

insert into smoking(s, p) values ('y', 0.5);
insert into smoking(s, p) values ('n', 0.5);


drop table if exists lungcancer;
create table lungcancer(l char(1), s char(1), p real);

insert into lungcancer(l, s, p) values ('y', 'y', 0.1);
insert into lungcancer(l, s, p) values ('y', 'n', 0.01);
insert into lungcancer(l, s, p) values ('n', 'y', 0.9);
insert into lungcancer(l, s, p) values ('n', 'n', 0.99);


drop table if exists bronchitis;
create table bronchitis(b char(1), s char(1), p real);

insert into bronchitis(b, s, p) values ('y', 'y', 0.6);
insert into bronchitis(b, s, p) values ('y', 'n', 0.3);
insert into bronchitis(b, s, p) values ('n', 'y', 0.4);
insert into bronchitis(b, s, p) values ('n', 'n', 0.7);


drop table if exists eithertublungorcancer;
create table eithertublungorcancer(e char(1), l char(1), t char(1), p real);

insert into eithertublungorcancer(e, l, t, p) values ('y', 'y', 'y', 1);
insert into eithertublungorcancer(e, l, t, p) values ('y', 'y', 'n', 1);
insert into eithertublungorcancer(e, l, t, p) values ('y', 'n', 'y', 1);
insert into eithertublungorcancer(e, l, t, p) values ('y', 'n', 'n', 0);
insert into eithertublungorcancer(e, l, t, p) values ('n', 'y', 'y', 0);
insert into eithertublungorcancer(e, l, t, p) values ('n', 'y', 'n', 0);
insert into eithertublungorcancer(e, l, t, p) values ('n', 'n', 'y', 0);
insert into eithertublungorcancer(e, l, t, p) values ('n', 'n', 'n', 1);


drop table if exists xray;
create table xray(x char(1), e char(1), p real);

insert into xray(x, e, p) values ('y', 'y', 0.98);
insert into xray(x, e, p) values ('y', 'n', 0.05);
insert into xray(x, e, p) values ('n', 'y', 0.02);
insert into xray(x, e, p) values ('n', 'n', 0.95);


drop table if exists dyspnoea;
create table dyspnoea(d char(1), e char(1), b char(1), p real);

insert into dyspnoea(d, e, b, p) values ('y', 'y', 'y', 0.9);
insert into dyspnoea(d, e, b, p) values ('y', 'y', 'n', 0.7);
insert into dyspnoea(d, e, b, p) values ('y', 'n', 'y', 0.8);
insert into dyspnoea(d, e, b, p) values ('y', 'n', 'n', 0.1);
insert into dyspnoea(d, e, b, p) values ('n', 'y', 'y', 0.1);
insert into dyspnoea(d, e, b, p) values ('n', 'y', 'n', 0.3);
insert into dyspnoea(d, e, b, p) values ('n', 'n', 'y', 0.3);
insert into dyspnoea(d, e, b, p) values ('n', 'n', 'n', 0.9);


drop view if exists joint;

create view joint as 
    select 
        a.a,
        t.t,
        s.s,
        l.l,
        e.e,
        b.b,
        d.d,
        x.x,
        a.p * t.p * s.p * l.p * e.p * b.p * d.p * x.p as p
    from
        asia a,
        tuberculosis t,
        smoking s,
        lungcancer l,
        eithertublungorcancer e,
        bronchitis b,
        dyspnoea d,
        xray x
    where
        t.a = a.a and
        l.s = s.s and
        b.s = s.s and
        e.t = t.t and
        e.l = l.l and
        d.e = e.e and
        d.b = b.b and
        x.e = e.e;


-- select t, sum(p) from joint where a='y' and s='n' and d='y' group by t;