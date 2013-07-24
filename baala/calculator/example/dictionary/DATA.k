#!/usr/bin/env koshu --run
** -*- koshu -*-
**
**  DESCRIPTION
**    Suppliers-and-parts dataset in Koshucode
**
**    This dataset is excerpt from Date's books,
**    e.g., "The Relational Database Dictionary"
**    or "An Introduction to Database Systems",
**    and translates into Koshucode.
**
**  USAGE
**    $ koshu DATA.k natural-join.k
**    $ ./DATA.k natural-join.k
**

**  Supplier

|-- S  /sno S1  /sname Smith  /status n 20  /city London
|-- S  /sno S2  /sname Jones  /status n 10  /city Paris
|-- S  /sno S3  /sname Blake  /status n 30  /city Paris
|-- S  /sno S4  /sname Clark  /status n 20  /city London
|-- S  /sno S5  /sname Adams  /status n 30  /city Athens

**  Part

|-- P  /pno P1  /pname Nut   /color Red   /weight n 12  /city London
|-- P  /pno P2  /pname Bolt  /color Green /weight n 17  /city Paris
|-- P  /pno P3  /pname Screw /color Blue  /weight n 17  /city Oslo
|-- P  /pno P4  /pname Screw /color Red   /weight n 14  /city London
|-- P  /pno P5  /pname Cam   /color Blue  /weight n 12  /city Paris
|-- P  /pno P6  /pname Cog   /color Red   /weight n 19  /city London

**  Shipment

|-- SP  /sno S1  /pno P1  /qty n 300
|-- SP  /sno S1  /pno P2  /qty n 200
|-- SP  /sno S1  /pno P3  /qty n 400
|-- SP  /sno S1  /pno P4  /qty n 200
|-- SP  /sno S1  /pno P5  /qty n 100
|-- SP  /sno S1  /pno P6  /qty n 100
|-- SP  /sno S2  /pno P1  /qty n 300
|-- SP  /sno S2  /pno P2  /qty n 400
|-- SP  /sno S3  /pno P2  /qty n 200
|-- SP  /sno S4  /pno P2  /qty n 200
|-- SP  /sno S4  /pno P4  /qty n 300
|-- SP  /sno S4  /pno P5  /qty n 400

