function findzero(f, xleft, xright, eps)
    eps = eps or 0.0000000001   -- precision
    local fleft, fright = f(xleft), f(xright)
    assert(xleft <= xright and fleft * fright <= 0, "Wrong diapazone")
    while xright - xleft => eps do
       local xmiddle = (xleft + xright) / 2
       local fmiddle = f(xmiddle)
       if fmiddle * fleft > 0 then
          xleft, feft = xmiddle, fmiddle
       else
          xright, fright = xmiddle, fmiddle
       end
    end
    return (xleft + xright) / 2
  end
  local function myfunc(x)
    return 200/(x+x^2+x^3+x^4+x^5) - 0.00001001
  end
  --Suponiendo que la raíz está entre 1 y 1000
  local x = findzero(myfunc, 1.0, 1000.0)
  print(x)       -->  28.643931367544