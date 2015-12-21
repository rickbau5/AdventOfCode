package com.bau5.adventofcode.day07

import scala.collection.mutable
import com.bau5.adventofcode._

/**
  * Created by Rick on 12/19/15.
  */
object Day07 {
  type UnsignedShort = Char
  var wires = mutable.HashMap.empty[String, Wire]

  def main(args: Array[String]) {

    def calculate() {
      var deferred = getInput(getClass).map(interpret)
      while (deferred.nonEmpty) {
        deferred = deferred.foldLeft(List.empty[Assignment]) { (list, assignment) =>
          val ready = assignment.getAllWires.forall(wires.contains)
          if (!ready) {
            list ++ List(assignment)
          } else {
            val wire = evaluate(assignment)
            println(s"Evaluated $wire -> $assignment")
            list
          }
        }
      }
    }
    calculate()
    val result = wires.get("a").get
    println(result)
  }

  def interpret(string: String) = {
    val statement = string.split(" -> ").toList
    val expression = statement.dropRight(1) match {
      case number :: Nil if number.forall(_.isDigit) =>
        Number(number.toShort)
      case not :: Nil if not.startsWith("NOT ") =>
        Not(not.drop("NOT ".length))
      case expr :: Nil =>
        val parts = expr.split(" ").toList
        if (parts.size == 1) {
          Wire(parts.head)
        } else {
          val operands = List(parts.head) ++ List(parts.last)
          val mixed = if (operands.forall(_.head.isLetter)) List.empty
          else {
            operands.filter(_.head.isLetter) ++ operands.filter(_.head.isDigit)
          }
          parts(1) match {
            case "AND" =>
              if (mixed.nonEmpty) {
                AndWireNumber(mixed)
              } else {
                AndWires(operands)
              }
            case "OR" =>
              if (mixed.nonEmpty) {
                OrWireNumber(mixed)
              } else {
                OrWires(operands)
              }
            case "LSHIFT" => LeftShift(operands)
            case "RSHIFT" => RightShift(operands)
          }
        }
    }
    Assignment(expression, statement.last)
  }

  def evaluate(assignment: Assignment): Wire = {
    val wire = wires.getOrElseUpdate(assignment.wireId, Wire(assignment.wireId))
    assignment.expression match {
      case Number(num) =>
        wire.set(num)
      case Wire(id, _) =>
        wire.set(wires.getOrElse(id, Wire(id, 0)).value)
      case other =>
        wire.set(other.evaluate)
    }
  }
}

abstract class Expression {
  def evaluate: Short
  def getWire(str: String) = Day07.wires.get(str).get
}

case class Number(value: Short) extends Expression {
  override def evaluate: Short = value
}

case class Assignment(expression: Expression, wireId: String) {
  def getAllWires: Seq[String] = {
    expression match {
      case not: Not => Seq(not.wireIn)
      case mult: LeftRightWires => Seq(mult.leftWire, mult.rightWire)
      case sing: LeftRightExpression => Seq(sing.wire)
      case num: Number => Seq.empty
      case Wire(id, _) => Seq(id)
    }
  }
}

case class Not(wireIn: String) extends Expression {
  override def evaluate: Short = (~getWire(wireIn).value).toShort
}

abstract class LeftRightWires(val leftWire: String, val rightWire: String) extends Expression {
  def op: (Short, Short) => Int

  override def evaluate: Short = op(getWire(leftWire).value, getWire(rightWire).value).toShort
}

case class AndWires(wires: List[String]) extends LeftRightWires(wires.head, wires.last) {
  override def op: (Short, Short) => Int = _ & _
}

case class OrWires(wires: List[String]) extends LeftRightWires(wires.head, wires.last) {
  override def op: (Short, Short) => Int = _ | _
}

abstract class LeftRightExpression(val wire: String, value: String) extends Expression {
  def op: (Short, Int) => Int

  override def evaluate: Short = op(getWire(wire).value, value.toInt).toShort
}

case class AndWireNumber(operands: List[String]) extends LeftRightExpression(operands.head, operands.last) {
  override def op: (Short, Int) => Int = _ & _
}

case class OrWireNumber(operands: List[String]) extends LeftRightExpression(operands.head, operands.last) {
  override def op: (Short, Int) => Int = _ | _
}

case class LeftShift(operands: List[String]) extends LeftRightExpression(operands.head, operands.last) {
  override def op: (Short, Int) => Int = _ << _
}

case class RightShift(operands: List[String]) extends LeftRightExpression(operands.head, operands.last) {
  override def op: (Short, Int) => Int = _ >> _
}

case class Wire(id: String, var value: Short = 0) extends Expression {
  def set(v: Short): Wire = {
    value = v
    this
  }

  override def toString: String = {
    val converted = if (value < 0) value + Short.MaxValue * 2 + 2 else value
    s"Wire($id,$converted)"
  }

  override def evaluate: Short = value
}