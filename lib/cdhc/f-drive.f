        program main
        real x(1000), y(2)

        do 10 i=1,1000
          read (*,*,end=20) x(i)
cc        print *, i, x(i)
  10    continue
  20    i=i-1
        print *,'N=',i
        call sort(i,x)
       call test1(x,y,i)
       call test2(x,y,i)
       call test3(x,y,i)
       call test4(x,y,i)
       call test5(x,y,i)
       call test6(x,y,i)
       call test7(x,y,i)
       call test8(x,y,i)
       call test9(x,y,i)
       call test10(x,y,i)
       call test11(x,y,i)
       call test12(x,y,i)
       call test13(x,y,i)
       call test14(x,y,i)
       call test15(x,y,i)
       call test16(x,y,i)
       call test17(x,y,i)
       call test18(x,y,i)
       call test19(x,y,i)
       call test20(x,y,i)
       call test21(x,y,i)
       call test22(x,y,i)
       call test23(x,y,i)
       call test24(x,y,i)
        end
