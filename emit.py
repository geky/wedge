
def flatten(ss):
    if ss is None:
        return ''
    elif isinstance(ss, str):
        return ss
    else:
        output = []
        for s in ss:
            if s is None:
                pass
            elif isinstance(s, str):
                output.append(s)
            else:
                lines = flatten(s).split('\n')
                output.extend('    '+l for l in lines)
        return '\n'.join(output)

def emit(ptree):
    lines = flatten(d.emit() for d in ptree).split('\n')
    return '\n'.join(l[4:] for l in lines)
   
 
