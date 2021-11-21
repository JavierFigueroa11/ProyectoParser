function average(...)
    result = 0
    local arg={...}
    for i,v in ipairs(arg) do
       result = result + v
    end
    print("Total entrante ",#arg," Numero/s")
    return result/#arg
end
print("El promedio es",average(10,5,3,4,5,6)))