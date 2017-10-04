package com.dotgoing.prac

import play.api.inject.{Binding, Module}
import play.api.{Configuration, Environment}

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 3:02 PM
  */
class MyModule extends Module {
  def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = {
    Seq(
      bind[MyComponent].to[MyComponentImpl]
    )
  }
}