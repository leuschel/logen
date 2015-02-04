


def scan_env(term, index, exit_char):
    term_length = len(term)
    while index < term_length  and term[index] != "'":
        index += 1
    return index



    
def get_arity(term):
    """
    Given argument list return the arity of a prolog function.
    eg (a,b,c) ==> (3, {1:(starta,enda,a),2:(startb,endb,b) ....
    """
    
    arity =0
    args = {}
    if len(term) == 0:
        return (0,None)
    
    if term[0] != '(':
        return (arity,None)
    #term = term[1:]

    #we have a list of arguments
    #we want to loop until we find a non nested ','

    index = 1
    lastargpos = 1
    #looks for all arguments
    term_length = len(term)
    while index < term_length:        
        brackets = 0

        ## find the next ',' that is not nested in brackets
        while (index < term_length) and (not (brackets == 0 and (term[index] == "," or term[index] ==')'))):
            if term[index] in  ['(','[']:                
                brackets += 1
            if term[index] in[')',']' ]:                
                brackets -= 1
            
            if term[index] == "'":
                index += 1
                index = scan_env(term, index, "'")
            if term[index] == '"':
                index += 1
                index = scan_env(term, index, '"')                                                                 
            index += 1

        if index == term_length:
            ##must have been some parse error...
            return (-1,None)
        
        arity += 1
        args[arity] = (lastargpos,index,term[lastargpos:index])
        lastargpos = index+1
        #we must be at the end...
        if term[index] == ')':
            return (arity,args)
        #otherwise we must have a ',' so we expect another argument
        index += 1

    #we couldn't parse it so we return -1
    return (-1,None);    


if __name__ == "__main__":
    print "Testing Arity Parser"

    test_cases = [
        "(a(b,c,d), (':-'(a,b,c)), d,e ) :- foo(bar, monkey)",
        "",
        "(a,b,((a)),d)",
        "(')',b,c,d)"
        ]
    
    for term in test_cases:
        (arity,args) = get_arity(term)
        print term
        print "Arity is %d" % arity
        if args is not None:
            for a in args:
                print "%d->%d is %s" % args[a]

    
