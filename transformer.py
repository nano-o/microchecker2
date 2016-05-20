# -*- coding: utf-8 -*-

import re

print('=====================================================================')

def scalaEquals(classname, arglist, argString):
  eString = argString
  if classname.startswith('seta') or classname.startswith('coset'):
    eString = ' && (' + arglist[0] + '.toSet == that.' + arglist[0] + '.toSet)'
  elif classname.startswith('finfun_update_code'):
    eString = ' && (setFinFun == finfun_to_set(that, finfun_to_dom(that)) && finfun_constv(this) == finfun_constv(that))'
  elif classname.startswith('acc_state') or classname.startswith('mp_state'):
    eString = ''
    for i in range(0, len(arglist) - 1):
      eString += ' && this.' + arglist[i] + ' == that.' + arglist[i]
  return eString

def scalaHash(classname, arglist, hashString):
  hString = hashString
  if len(arglist) == 1:
    hString = arglist[0] + '.hashCode()'
    if classname.startswith('seta') or classname.startswith('coset'):
      hString = arglist[0] + '.toSet.hashCode()'
  else:
    if classname.startswith('finfun_update_code'):
      hString = '  41 * ( 41 + finfun_constv(this).hashCode()) + setFinFun.hashCode()'
    elif classname.startswith('acc_state') or classname.startswith('mp_state'):
      hString = '1'
      for i in range(0, len(arglist) - 1):
        hString = '  41 * (' + hString + ') + ' + arglist[i] + '.hashCode()'
  return hString

def scalaToString(classname, arglist):
  tString = '"' + classname + '(" + ' + arglist[0] + '.toString()'
  for i in range(1, len(arglist)):
    tString += ' + ", " + ' + arglist[i] + '.toString()'
  tString +=  ' + ")"'
  if classname.startswith('Nat'):
    tString = arglist[0] + '.toString()'
  if classname.startswith('mp_state'):
    tString = '"node_states:\\n" + ' + arglist[0] + '.toString() + "\\n" + "network: " + ' + arglist[1] + '.toString()'
  elif classname.startswith('finfun_const'):
    tString = '"[default |-> " + ' + arglist[0] + '.toString() + "]"'
  elif classname.startswith('finfun_update_code'):
    tString = 'print_finfun_set(setFinFun) + "[default |-> " + finfun_constv(' + arglist[0] + ').toString() + "]"'
  elif classname.startswith('seta') or classname.startswith('coset'):
    tString = arglist[0] + '.mkString("{",",","}")'
  elif classname.startswith('Abs_fset'):
    tString = arglist[0] + '.toString()'
  elif classname.startswith('acc_state'):
    tString = '"id: " + ' + arglist[0] + '.toString() + "    leader: " + ' + arglist[1] + '.toString() + ",    acceptors: " + ' + arglist[2] + '.toString() + ",    ballot: " + ' + arglist[3] + '.toString() + ",    decided: " + ' + arglist[4] + '.toString() + ",    vote: " + ' + arglist[5] + '.toString() + ",    last_ballot: " + ' + arglist[6] + '.toString() + "\\n" + "    onebs: " + ' + arglist[7] + '.toString() + ",    twobs: " + ' + arglist[8] + '.toString() + "\\n" + "    next_inst: " + ' + arglist[9] + '.toString() + ",    last_decision: " + ' + arglist[10] + '.toString() + ",    working_instances: " + ' + arglist[11] + '.toString() + ",    commit_buffer: " + ' + arglist[12] + '.toString() + ",    last_commited: " + ' + arglist[13] + '.toString() + ",    snapshot_reference: " + ' + arglist[14] + '.toString() + ",    snapshot_proposal: " + ' + arglist[15] + '.toString() + "\\n"'
  return tString

filename = 'MultiPaxos4.scala'
nfilename = 'MultiPaxos.scala'

#Read the content of the file
scalafile = open(filename, 'r')
nfile = open(nfilename, 'w+')
open(nfilename, 'w').close()
nfile = open(nfilename, 'a+')

nfile.write('package microchecker\n')
nfile.write('\n')

line = scalafile.readline() 

while line:
  newline = line
  if line.lstrip().startswith('final case class'):
    while re.search('extends', line) == None:
      line = scalafile.readline()
      newline += line
    if re.search('(?<=extends)[a-zA-Z0-9\_\,\[\]]+', line.replace(" ", "")) == None:
      line = scalafile.readline()
      newline += line

    nfile.write(newline)
    nfile.write('{\n')

    classname = re.findall('(?<=final case class\s)[a-zA-Z0-9\_\s\,\[\]]+', newline)[0]
    classname = classname.replace(" ", "")
    subclassname = re.findall('[a-zA-Z0-9\_]+', classname)[0]
    print(subclassname)

    arg = re.findall('(?<=\()[A-Za-z0-9\:\,\[\(\]\.\_\n\)]+extends',newline.replace(" ", ""))

    if arg == [')extends']:
      nfile.write('  override def equals(other: Any) = other match {\n    case that:' + classname + ' => (that.isInstanceOf[' + classname +'])'
        '\n    case _ => false\n  }\n')
      nfile.write('  override def toString = "' + classname.split('[')[0] + '"\n')
      nfile.write('  override def hashCode : Int = 41\n')
    else:
      argAll = arg[0].split(":")
    
      arglist = []
      argString = ''
      hashString = '1'

      for i in range(0, len(argAll) - 1):
        argElem = argAll[i].split(",")
        arglist.append(argElem[len(argElem) - 1].strip())
        argString += ' && this.' + arglist[i] + ' == that.' + arglist[i]
        hashString = '  41 * (' + hashString + ') + ' + arglist[i] + '.hashCode()'

      if "finfun_update_code" in classname:
        nfile.write('  val setFinFun = finfun_to_set(this, finfun_to_dom(this))\n')
      nfile.write('  override def equals(other: Any) = other match {\n    case that:' + classname + ' => (that.isInstanceOf[' + classname +'])' + scalaEquals(subclassname, arglist, argString)
        + '\n    case _ => false\n  }\n')
      nfile.write('  override def toString = ' + scalaToString(subclassname, arglist) + '\n')
      nfile.write('  override def hashCode : Int = ' + scalaHash(subclassname, arglist, hashString) + '\n')
    nfile.write('}\n')
    if "finfun_update_code" in classname:
      nfile.write('\ndef finfun_to_dom[A, B](x0: finfun[A, B]): Set[A] = x0 match {\n  case finfun_update_code(f, a, b) => (\n    if (eq[B](b, finfun_constv[A, B](f)))\n      finfun_to_dom[A, B](f) - a\n    else\n      finfun_to_dom[A, B](f) + a)\n  case finfun_const(c) => Set()\n}\n\ndef finfun_to_set[A, B](x0: finfun[A, B], domA: Set[A]): Set[(A,B)] = {\n  var setFinFun: Set[(A,B)] = Set()\n  domA.foreach { a => setFinFun += Tuple2(a, finfun_apply(x0, a)) }\n  setFinFun\n}\n\ndef print_finfun_set[A,B](setfinfun: Set[(A,B)]): String = {\n  var strfinfun : String = ""\n  setfinfun.foreach { case (a,b) => (\n      strfinfun += "[" + a.toString() + " |-> " + b.toString() + "]"\n    )}\n  strfinfun\n}\n\ndef finfun_constv[A, B](x0: finfun[A, B]): B = x0 match {\n  case finfun_update_code(f, a, b) => finfun_constv[A, B](f)\n  case finfun_const(c) => c\n}\n')
  elif "implicit def equal_nat" in line:
    nfile.write("implicit def equal_t[A] : equal[A] = new equal[A] {\n  val `MicroCheckerLib.equal` = (a : A, b: A) => a == b\n}\n\n")
    nfile.write(line)
  else:
    nfile.write(line)
  line = scalafile.readline()

scalafile.close()
nfile.close()


