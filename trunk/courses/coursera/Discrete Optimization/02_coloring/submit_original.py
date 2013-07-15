
# C:\UBS\dev\courses\coursera\Discrete Optimization\02_coloring>python C:\UBS\dev\portablepython\App\Scripts\uncompyle2 submit.pyc 
# 2013.07.01 15:19:01 Central European Daylight Time
#Embedded file name: submit.py
import urllib
import urllib2
import hashlib
import email.message
import email.encoders
import time

class NullDevice:

    def write(self, s):
        pass


def checkLogin(login, password):
    sid = 'B5DXTczU-dev'
    submission = '0'
    source = ''
    print '== Checking Login Credentials ... '
    login, ch, state, ch_aux = getChallenge(login, sid)
    if not login or not ch or not state:
        print '\n!! Error: %s\n' % login
        return
    ch_resp = challengeResponse(login, password, ch)
    result, string = submitSolution(login, ch_resp, sid, submission, source, state, ch_aux)
    if string.strip() == 'password verified':
        print '== credentials verified'
    else:
        print '\n!! login failed'
        print '== %s' % string.strip()
        quit()


def submit():
    print '==\n== Graph Coloring Solution Submission \n=='
    login, password = loginPrompt()
    if not login:
        print '!! Submission Cancelled'
        return
    print '\n== Connecting to Coursera ... '
    checkLogin(login, password)
    parts = partPrompt()
    for partIdx, sid in parts:
        login, ch, state, ch_aux = getChallenge(login, sid)
        if not login or not ch or not state:
            print '\n!! Error: %s\n' % login
            return
        submission = output(partIdx)
        ch_resp = challengeResponse(login, password, ch)
        result, string = submitSolution(login, ch_resp, sid, submission, source(partIdx), state, ch_aux)
        print '== %s' % string.strip()


def loginPrompt():
    """Prompt the user for login credentials. Returns a tuple (login, password)."""
    login, password = basicPrompt()
    return (login, password)


def basicPrompt():
    """Prompt the user for login credentials. Returns a tuple (login, password)."""
    login = raw_input('Login (Email address): ')
    password = raw_input("Submission Password (from the programming assignments page. This is NOT your own account's password): ")
    return (login, password)


def partPrompt():
    print 'Hello! These are the assignment parts that you can submit:'
    counter = 0
    for part in partFriendlyNames:
        counter += 1
        print str(counter) + ') ' + partFriendlyNames[counter - 1]

    print '0) All'
    partText = raw_input('Please enter which part(s) you want to submit (0-' + str(counter) + '): ')
    partIdx = set()
    for item in partText.split(','):
        try:
            i = int(item) - 1
        except:
            print 'Skipping input "' + item + '".  It is not an integer.'
            continue

        if i >= counter:
            print 'Skipping input "' + item + '".  It is out of range (the maximum value is ' + str(counter) + ').'
            continue
        if i < 0:
            partIdx |= set(range(0, len(partIds)))
        else:
            partIdx.add(i)

    if len(partIdx) <= 0:
        print 'No valid assignment parts identified.  Please try again. \n'
        return partPrompt()
    else:
        return map(lambda x: (x, partIds[x]), partIdx)


def getChallenge(email, sid):
    """Gets the challenge salt from the server. Returns (email,ch,state,ch_aux)."""
    url = challenge_url()
    values = {'email_address': email,
     'assignment_part_sid': sid,
     'response_encoding': 'delim'}
    data = urllib.urlencode(values)
    req = urllib2.Request(url, data)
    response = urllib2.urlopen(req)
    text = response.read().strip()
    splits = text.split('|')
    if len(splits) != 9:
        print 'Badly formatted challenge response: %s' % text
        return None
    else:
        return (splits[2],
         splits[4],
         splits[6],
         splits[8])


def challengeResponse(email, passwd, challenge):
    sha1 = hashlib.sha1()
    sha1.update(''.join([challenge, passwd]))
    digest = sha1.hexdigest()
    strAnswer = ''
    for i in range(0, len(digest)):
        strAnswer = strAnswer + digest[i]

    return strAnswer


def challenge_url():
    """Returns the challenge url."""
    return 'https://class.coursera.org/' + URL + '/assignment/challenge'


def submit_url():
    """Returns the submission url."""
    return 'https://class.coursera.org/' + URL + '/assignment/submit'


def submitSolution(email_address, ch_resp, sid, output, source, state, ch_aux):
    """Submits a solution to the server. Returns (result, string)."""
    source_64_msg = email.message.Message()
    source_64_msg.set_payload(source)
    email.encoders.encode_base64(source_64_msg)
    output_64_msg = email.message.Message()
    output_64_msg.set_payload(output)
    email.encoders.encode_base64(output_64_msg)
    values = {'assignment_part_sid': sid,
     'email_address': email_address,
     'submission': output_64_msg.get_payload(),
     'submission_aux': source_64_msg.get_payload(),
     'challenge_response': ch_resp,
     'state': state}
    url = submit_url()
    data = urllib.urlencode(values)
    req = urllib2.Request(url, data)
    response = urllib2.urlopen(req)
    string = response.read().strip()
    result = 0
    return (result, string)


def source(partIdx):
    f = open(sourceFiles[partIdx])
    src = f.read()
    f.close()
    return src


try:
    pkg = __import__('solver')
    if not hasattr(pkg, 'solveIt'):
        print 'the solveIt() function was not found in solver.py'
        quit()
    solveIt = pkg.solveIt
except ImportError:
    print 'solver.py was not found in the pyhton path.'
    quit()

def loadInputData(fileLocation):
    inputDataFile = open(fileLocation, 'r')
    inputData = ''.join(inputDataFile.readlines())
    inputDataFile.close()
    return inputData


URL = 'optimization-001'
partIds = ['upNcCDtk',
 'Cy6WMCm7',
 'UOKSlXMq',
 'ecjBwfpP',
 'a10G1b1U',
 'pIzekcCG']
partFriendlyNames = ['Coloring Problem 1',
 'Coloring Problem 2',
 'Coloring Problem 3',
 'Coloring Problem 4',
 'Coloring Problem 5',
 'Coloring Problem 6']
sourceFiles = ['solver.py',
 'solver.py',
 'solver.py',
 'solver.py',
 'solver.py',
 'solver.py']
fileNameLookup = {'upNcCDtk': './data/gc_50_3',
 'Cy6WMCm7': './data/gc_70_7',
 'UOKSlXMq': './data/gc_100_5',
 'ecjBwfpP': './data/gc_250_9',
 'a10G1b1U': './data/gc_500_1',
 'pIzekcCG': './data/gc_1000_5',
 'upNcCDtk-dev': './data/gc_50_3',
 'Cy6WMCm7-dev': './data/gc_70_7',
 'UOKSlXMq-dev': './data/gc_100_5',
 'ecjBwfpP-dev': './data/gc_250_9',
 'a10G1b1U-dev': './data/gc_500_1',
 'pIzekcCG-dev': './data/gc_1000_5'}

def output(partIdx):
    """Uses the student code to compute the output for test cases."""
    solution = ''
    start = time.clock()
    try:
        solution = solveIt(loadInputData(fileNameLookup[partIds[partIdx]]))
    except Exception as e:
        print 'the solveIt(inputData) method from solver.py raised an exception'
        print 'try testing it with python ./solver.py before running this submission script'
        print 'exception message:'
        print e
        print ''
        return 'Local Exception =('

    end = time.clock()
    if not isinstance(solution, str):
        print 'Warning: the submitted solution was not ASCII and will be converted.  Some information may be lost.'
        print 'Orginal: '
        print solution
        solution = solution.encode('ascii', 'ignore')
    print 'Submitting: '
    print solution
    return solution.strip() + '\n' + str(end - start)


submit()
# okay decompyling submit.pyc 
# decompiled 1 files: 1 okay, 0 failed, 0 verify failed
# 2013.07.01 15:19:02 Central European Daylight Time
