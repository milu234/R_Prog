empid = c(1:10)
empname = c("Anurag", "Rahul" ,"Nilesh", "Chirag", "Athul", "Manoj", "Raj","Abhishek","Rajpreet", "Sumeet")
salary = ceiling(runif(10, 30000 , 70000))
dno = factor(rep(c(101:105),length = 10))
emp = data.frame(empid, empname, salary, dno, stringsAsFactors = F)
emp

dno = c(101:105)
dname = c("IT", "Sales" , "Manufacturing" , "Marketing" , "Finance")
dept = data.frame(dno, dname)
dept

#Display empid of all employees who are working for department 101.
data_temp = emp[emp$dno == 101,]
data_temp['empid']

#Display empid,empname of employees who are earning salary more than 50000 and working for  IT department
temp_dno = dept[dept$dname == 'IT' , 'dno_pk']
temp_data2 = emp[emp$dno == temp_dno,]
temp_data2
result = temp_data2[temp_data2$salary >= 50000,]
result["empname"]

#Display employee details of employees working of IT department
temp_dno = dept[dept$dname == 'IT' , 'dno_pk']
temp_data2 = emp[emp$dno == temp_dno,]
temp_data2

#Display dno and number of employees working for every department.
aggregate(emp['empid'],list(Department_Number = emp$dno), FUN = length)
summarise(dept)

#Display empname and dname of department for which emp is working.
temp_data = merge(x = emp , y = dept, by = 'dno',all = FALSE)
res = data.frame(temp_data$empname, temp_data$dname)
res

#Give examples of different types of Joins.

#Inner Join
temp_data = merge(x = emp , y = dept, by = 'dno')
temp_data

#Outer Join
temp_data = merge(x = emp , y = dept, by = 'dno',all = TRUE)
temp_data

#Left Outer Join
temp_data = merge(x = emp , y = dept, by = 'dno',all.x = TRUE)
temp_data

#Right Outer Join
temp_data4 = merge(x = emp , y = dept, by = 'dno',all.y = TRUE)
temp_data4

#Cross Join
temp_data4 = merge(x = emp , y = dept, by = NULL)
temp_data4






