package com.bitrock

import org.scalatest.{BeforeAndAfterEach, EitherValues}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait BaseSpec extends AnyWordSpec with Matchers with BeforeAndAfterEach with EitherValues
