** -*- koshu -*-
**  
**  INPUT
**    DATA.k
**    commutative.k
**    

|-- S-SP  /pno P6  /qty n 100  /sno S1  /sname Smith  /status n 20  /city London
|-- S-SP  /pno P5  /qty n 100  /sno S1  /sname Smith  /status n 20  /city London
|-- S-SP  /pno P4  /qty n 200  /sno S1  /sname Smith  /status n 20  /city London
|-- S-SP  /pno P3  /qty n 400  /sno S1  /sname Smith  /status n 20  /city London
|-- S-SP  /pno P2  /qty n 200  /sno S1  /sname Smith  /status n 20  /city London

|-- S-SP  /pno P1  /qty n 300  /sno S1  /sname Smith  /status n 20  /city London
|-- S-SP  /pno P2  /qty n 400  /sno S2  /sname Jones  /status n 10  /city Paris
|-- S-SP  /pno P1  /qty n 300  /sno S2  /sname Jones  /status n 10  /city Paris
|-- S-SP  /pno P2  /qty n 200  /sno S3  /sname Blake  /status n 30  /city Paris
|-- S-SP  /pno P5  /qty n 400  /sno S4  /sname Clark  /status n 20  /city London

|-- S-SP  /pno P4  /qty n 300  /sno S4  /sname Clark  /status n 20  /city London
|-- S-SP  /pno P2  /qty n 200  /sno S4  /sname Clark  /status n 20  /city London
|-- SP-S  /sname Smith  /status n 20  /city London  /sno S1  /pno P1  /qty n 300
|-- SP-S  /sname Smith  /status n 20  /city London  /sno S1  /pno P2  /qty n 200
|-- SP-S  /sname Smith  /status n 20  /city London  /sno S1  /pno P3  /qty n 400

|-- SP-S  /sname Smith  /status n 20  /city London  /sno S1  /pno P4  /qty n 200
|-- SP-S  /sname Smith  /status n 20  /city London  /sno S1  /pno P5  /qty n 100
|-- SP-S  /sname Smith  /status n 20  /city London  /sno S1  /pno P6  /qty n 100
|-- SP-S  /sname Jones  /status n 10  /city Paris  /sno S2  /pno P1  /qty n 300
|-- SP-S  /sname Jones  /status n 10  /city Paris  /sno S2  /pno P2  /qty n 400
**  (20 judges)

|-- SP-S  /sname Blake  /status n 30  /city Paris  /sno S3  /pno P2  /qty n 200
|-- SP-S  /sname Clark  /status n 20  /city London  /sno S4  /pno P2  /qty n 200
|-- SP-S  /sname Clark  /status n 20  /city London  /sno S4  /pno P4  /qty n 300
|-- SP-S  /sname Clark  /status n 20  /city London  /sno S4  /pno P5  /qty n 400
**  (24 judges)

